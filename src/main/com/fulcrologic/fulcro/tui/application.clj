(ns com.fulcrologic.fulcro.tui.application
  "The application/lifecycle front door for the TUI rendering target: build a Fulcro app, attach a
   terminal, run the input loop, and stop it. This is also where the side-effecting render driver
   lives (it renders a Fulcro app to a terminal and runs the keyboard input loop).

   This is the edge that ties together the pure TUI pipeline in `com.fulcrologic.fulcro.tui.engine`
   (layout, paint, diff, focus, input) and the element generators in
   `com.fulcrologic.fulcro.tui.elements` with a concrete `com.fulcrologic.fulcro.tui.terminal/Terminal`. Use
   `application` to build a synchronous raw Fulcro app whose renders repaint the terminal, `mount!`
   (or `run-blocking!`) to attach a terminal and start the keyboard input loop, `quit!` to stop it,
   and `step!` to drive a single deterministic iteration (used by tests).

   State/runtime keys (single source of truth):
     * Focus & carets & scroll are owned by `com.fulcrologic.fulcro.tui.engine` (see that ns).
     * The attached terminal and the bookkeeping for incremental painting live in the app
       RUNTIME-ATOM under this namespace's keys (see `::terminal`, `::prev-buffer`, `::placed`,
       `::last-size`).

   This is JVM/babashka only (plain `.clj`).

   Viewport scrolling: `engine/place` lays a viewport's single child out at its natural size into a
   virtual rect, and `engine/render-buffer` blits only the window `[scroll-x scroll-y w h]` of that
   virtual content into the viewport's `::rect`. Scroll offsets are stored in the state-map under
   `:com.fulcrologic.fulcro.tui.engine/scroll` keyed by viewport id; `render!` injects them onto the placed
   tree (`inject-scroll`), `follow-focus!` advances them after focus changes to keep the focused node
   visible, and PageUp/PageDown page-scroll via `viewport-scroll-key!`. Up/Down arrows move focus
   item-to-item (in `engine/process-key!`) and `follow-focus!` autoscrolls to track the focused item."
  (:require
   [clojure.spec.alpha :as s]
   [com.fulcrologic.fulcro.algorithms.denormalize :as fdn]
   [com.fulcrologic.fulcro.algorithms.tx-processing.synchronous-tx-processing :as stx]
   [com.fulcrologic.fulcro.raw.application :as rapp]
   [com.fulcrologic.fulcro.raw.components :as rc]
   [com.fulcrologic.fulcro.tui.engine :as engine]
   [com.fulcrologic.fulcro.tui.elements :as elements]
   [com.fulcrologic.fulcro.tui.terminal :as term]
   [com.fulcrologic.guardrails.core :refer [>def >defn >defn- => ?]]))

;; ============================================================================
;; Runtime keys
;; ============================================================================

(>def ::terminal (? any?))
(>def ::prev-buffer (? map?))
(>def ::placed (? map?))
(>def ::last-size (? map?))
(>def ::handle map?)
(>def ::min-size (s/keys :req-un [::min-width ::min-height]))
(>def ::min-width int?)
(>def ::min-height int?)
(>def ::caret-pos (s/tuple int? int? boolean?))

(def ^:private runtime-atom-key :com.fulcrologic.fulcro.application/runtime-atom)
(def ^:private state-atom-key :com.fulcrologic.fulcro.application/state-atom)
(def ^:private root-class-key :com.fulcrologic.fulcro.application/root-class)

(>defn- runtime
        "Returns the (dereferenced) runtime map for `app`, or `nil`."
        [app]
        [any? => (? map?)]
        (when-let [ra (get app runtime-atom-key)]
          (deref ra)))

(>defn terminal
       "Returns the terminal currently attached to `app` (from its runtime atom), or `nil`."
       [app]
       [any? => (? any?)]
       (::terminal (runtime app)))

;; ============================================================================
;; Render (the custom optimized paint)
;; ============================================================================

(>defn- root-min
        "Returns `{:min-width W :min-height H}` for the root `node-tree`, reading the `:min-width`/
   `:min-height` attrs of the root node and defaulting each to 1."
        [node-tree]
        [any? => ::min-size]
        {:min-width  (long (or (engine/node-attr node-tree :min-width) 1))
         :min-height (long (or (engine/node-attr node-tree :min-height) 1))})

(>defn too-small-buffer
       "Returns a `rows`x`cols` cell buffer painted with a centered \"terminal too small\" message asking
   for at least `min-w` x `min-h`. Used when the terminal is smaller than the root's declared minimum."
       [rows cols min-w min-h]
       [nat-int? nat-int? int? int? => map?]
       (let [msg (str "terminal too small — need " min-w "x" min-h)
             node (engine/place
                   (elements/text {} msg)
                   {:x 0 :y 0 :w cols :h rows})]
         (engine/render-buffer node rows cols)))

(>defn- caret-screen-position
        "Returns `[x y visible?]` for the hardware cursor given the placed `focused-node` (or `nil`), its
   effective on-screen `rect` (or `nil` when the node has none / is scrolled out of view), and the
   caret index `caret`.

   For a single-line `:input`, the cursor is placed at the rect origin advanced by the display width of
   the value up to `caret`, clamped to lie within the rect, and visible.

   For a multiline `:input` (`engine/multiline-input?`), the value is wrapped to the rect's width, the
   caret's visual `[row col]` is computed (`engine/caret->rowcol`), and the cursor is placed at
   `rect-origin + (row - top-line, col)` where `top-line` is the input's injected internal scroll
   (`::engine/text-scroll`). If that visual row is scrolled out of the rect's `[0, h)` window the cursor
   is hidden.

   For any other focused node the cursor is placed (visible) at the rect origin. When `rect` is `nil`
   the cursor is hidden at the origin."
        [focused-node rect caret]
        [(? map?) (? map?) int? => ::caret-pos]
        (if rect
          (cond
            (engine/multiline-input? focused-node)
            (let [value     (str (engine/node-attr focused-node :value))
                  width     (:w rect)
                  top       (max 0 (long (or (::engine/text-scroll focused-node) 0)))
                  [row col] (engine/caret->rowcol value width caret)
                  vy        (- row top)
                  visible?  (and (>= vy 0) (< vy (:h rect)))
                  max-x     (max (:x rect) (+ (:x rect) (dec (:w rect))))
                  x         (min max-x (+ (:x rect) col))
                  y         (+ (:y rect) vy)]
              [x y visible?])

            (= :input (::engine/tag focused-node))
            (let [value   (str (engine/node-attr focused-node :value))
                  c       (max 0 (min caret (count value)))
                  advance (engine/string-width (subs value 0 c))
                  max-x   (max (:x rect) (+ (:x rect) (dec (:w rect))))
                  x       (min max-x (+ (:x rect) advance))
                  y       (:y rect)]
              [x y true])

            :else [(:x rect) (:y rect) true])
          [0 0 false]))

(>defn- focused-screen-rect
        "Returns the on-screen `::engine/rect` for the focused node `focus-id` within `placed`, accounting for an
   enclosing scrolled viewport, or `nil` when the focused node is scrolled out of its viewport's window.
   For a node NOT inside a viewport, returns the node's own placed `::engine/rect` (its absolute rect).
   For a node inside a viewport, its on-screen rect is `viewport-content-origin + (virtual-origin -
   scroll)`; if that lands outside the viewport's content window, `nil` is returned (cursor hidden)."
        [app placed focus-id]
        [any? any? any? => (? map?)]
        (if-let [{:keys [viewport virtual-rect]} (engine/focus-viewport-context placed focus-id)]
          (let [vp-id   (engine/node-attr viewport :id)
                scroll  (if (some? vp-id) (engine/viewport-scroll app vp-id) (or (::engine/scroll viewport) {:x 0 :y 0}))
                vsize   (::engine/virtual-size viewport)
                cv      (engine/content-view-size viewport)
                scroll  (engine/clamp-scroll scroll vsize cv)
                vp-rect (::engine/rect viewport)
                e       (+ (long (or (:padding (::engine/attrs viewport)) 0)) (if (:border? (::engine/attrs viewport)) 1 0))
                ox      (+ (:x vp-rect) e)
                oy      (+ (:y vp-rect) e)
                sx      (+ ox (- (:x virtual-rect) (:x scroll)))
                sy      (+ oy (- (:y virtual-rect) (:y scroll)))]
            (when (and (>= sy oy) (< sy (+ oy (:h cv)))
                       (>= sx ox) (< sx (+ ox (:w cv))))
              {:x sx :y sy :w (:w virtual-rect) :h (:h virtual-rect)}))
          (some-> (engine/find-by-id placed focus-id) ::engine/rect)))

(>defn- position-cursor!
        "Positions the hardware cursor of `terminal` for `app` against the `placed` tree: finds the focused
   node (`engine/current-focus`), computes its on-screen rect (via `focused-screen-rect`, which accounts
   for a scrolled enclosing viewport and hides the cursor when the focused node is scrolled out of
   view), computes its caret position, and calls `term/t-set-cursor!`. Returns `app`."
        [app terminal placed]
        [any? any? any? => any?]
        (let [focus-id     (engine/current-focus app)
              focused-node (when (some? focus-id) (engine/find-by-id placed focus-id))
              screen-rect  (when (some? focus-id) (focused-screen-rect app placed focus-id))
              value        (str (engine/node-attr focused-node :value))
              caret        (if (some? focus-id) (engine/get-caret app focus-id (count value)) 0)
              [x y vis?]   (caret-screen-position focused-node screen-rect caret)]
          (term/t-set-cursor! terminal x y vis?)
          app))

(>defn- inject-scroll
        "Returns the placed `tree` with scroll state injected for the next paint, and records each multiline
   input's effective wrap width on `app`'s runtime (`engine/set-input-width!`).

   For every `:viewport` node it sets `::engine/scroll` from `app`'s scroll state (the state-map key
   `::engine/scroll`, keyed by viewport id), clamped via `engine/clamp-scroll` against the viewport's
   `::engine/virtual-size` and its content-area view size. Viewports without an `:id` keep their default
   `{:x 0 :y 0}`.

   For every multiline `:input` node (`engine/multiline-input?`) it records the input's content width
   (its placed content-area width) so key handling wraps at the painted width, then computes the
   internal top visual-line offset (`engine/text-scroll-top`) from the input's caret so the caret row
   stays visible, and assocs it under `::engine/text-scroll`.

   Walks the placed tree (including nested viewport content)."
        [app tree]
        [any? any? => any?]
        (letfn [(walk [x]
                  (if (engine/node? x)
                    (let [x (cond
                              (engine/viewport? x)
                              (let [id     (engine/node-attr x :id)
                                    scroll (if (some? id) (engine/viewport-scroll app id) (::engine/scroll x))
                                    scroll (engine/clamp-scroll scroll (::engine/virtual-size x) (engine/content-view-size x))]
                                (assoc x ::engine/scroll scroll))

                              (engine/multiline-input? x)
                              (let [id    (engine/node-attr x :id)
                                    width (:w (engine/content-view-size x))
                                    value (str (engine/node-attr x :value))
                                    h     (:h (engine/content-view-size x))
                                    caret (if (some? id) (engine/get-caret app id (count value)) (count value))]
                                (when (some? id) (engine/set-input-width! app id width))
                                (assoc x ::engine/text-scroll (engine/text-scroll-top value width caret h)))

                              :else x)
                          x (update x ::engine/children (fn [cs] (mapv walk cs)))]
                      (if-let [vc (::engine/viewport-content x)]
                        (assoc x ::engine/viewport-content (walk vc))
                        x))
                    x))]
          (walk tree)))

(>defn follow-focus!
       "Adjusts viewport scroll state so the currently focused node stays visible, then returns `app`.
   Using the `placed` tree, finds the focused node's enclosing viewport (`engine/focus-viewport-context`)
   and the focused node's VIRTUAL rect. Computes the minimal scroll that brings that rect into the
   viewport's content window (`engine/scroll-to-show`), clamps it (`engine/clamp-scroll`), and writes it to
   the viewport's scroll state (`engine/set-viewport-scroll!`) when it differs. A no-op when the focused
   node is not inside a viewport or the enclosing viewport has no `:id`."
       [app placed]
       [any? any? => any?]
       (let [focus-id (engine/current-focus app)]
         (when (some? focus-id)
           (when-let [{:keys [viewport virtual-rect]} (engine/focus-viewport-context placed focus-id)]
             (when-let [vp-id (engine/node-attr viewport :id)]
               (let [vsize     (::engine/virtual-size viewport)
                     cr        (engine/content-view-size viewport)
                     current   (engine/viewport-scroll app vp-id)
                     desired   (engine/clamp-scroll (engine/scroll-to-show current virtual-rect cr) vsize cr)]
                 (when (not= current desired)
                   (engine/set-viewport-scroll! app vp-id desired))))))
         app))

(>defn- viewport-scroll-key!
        "If `key-event` is `:page-up`/`:page-down` and the currently focused node lies inside a viewport,
   scrolls that viewport's scroll state by a page and returns truthy (`:handled`). Returns `nil`
   (unhandled) otherwise, so the caller falls through to the normal key pipeline. `placed` is the
   current placed tree (for locating the enclosing viewport).

   `:up`/`:down` are NOT handled here: they drive focus navigation in `engine/process-key!` (moving
   focus item-to-item through the focus ring), and `follow-focus!` keeps the focused item visible —
   so arrowing through a focusable list autoscrolls its viewport. PageUp/PageDown remain the explicit
   page-scroll for a focused viewport."
        [app placed key-event]
        [any? any? map? => any?]
        (let [k        (:key key-event)
              focus-id (engine/current-focus app)
              ctx      (when (some? focus-id) (engine/focus-viewport-context placed focus-id))
              viewport (:viewport ctx)
              vp-id    (when viewport (engine/node-attr viewport :id))]
          (when (and viewport (some? vp-id))
            (let [vsize  (::engine/virtual-size viewport)
                  cr     (engine/content-view-size viewport)
                  view-h (:h cr)
                  page   (max 1 (dec view-h))
                  cur    (engine/viewport-scroll app vp-id)
                  dy     (cond
                           (= k :page-down) page
                           (= k :page-up)   (- page)
                           :else            nil)]
              (when dy
                (let [next (engine/clamp-scroll (update cur :y + dy) vsize cr)]
                  (engine/set-viewport-scroll! app vp-id next)
                  :handled))))))

(>defn render!
       "Paints `app` to its attached terminal. This is the driver's core render and is wired as the app's
   render hook so any state change repaints.

   It (1) computes the pure node tree from app state (root class + `db->tree`), (2) reads the terminal
   size, (3) lays the tree out into a placed tree and paints it into a fresh cell buffer (or, when the
   terminal is below the root's declared `:min-width`/`:min-height`, a 'too small' buffer), (4) diffs
   against the previously-painted buffer and writes the resulting ANSI to the terminal (wrapping in a
   synchronized-output frame when the terminal supports it), (5) positions the hardware cursor at the
   focused input's caret (or hides/origins it otherwise), and (6) stashes the new buffer, placed tree,
   and terminal size in the runtime atom for the next frame. A change in terminal size invalidates the
   previous buffer so the next frame is a full repaint. A no-op when no terminal is attached. Returns
   `app`."
       [app]
       [any? => any?]
       (let [rt       (runtime app)
             terminal (::terminal rt)]
         (if (nil? terminal)
           app
      ;; Serialize renders: the render hook fires on the input thread (via transactions) while the
      ;; terminal's resize handler fires on JLine's signal thread — both call `render!`, and they must
      ;; not interleave their writes to the terminal or the cached prev-buffer/placed-tree.
           (locking terminal
             (let [state-map      (some-> app state-atom-key deref)
                   root-class     (root-class-key rt)
                   query          (rc/get-query root-class state-map)
                   tree           (fdn/db->tree query state-map state-map)
                   ;; `engine/render-root` binds Fulcro's render-time dynamic vars (comp/*app* etc.)
                   ;; itself from the `app` arg; here we only need the TUI-specific focus var so
                   ;; `elements/focused?` resolves during the component renders.
                   full-tree      (binding [engine/*current-focus* (engine/current-focus app)]
                                    (engine/render-root root-class tree app))
                   ;; Overlays (open `:modal` nodes) are composited on top of the base UI; the base is
                   ;; laid out from the tree with all modals stripped, and the topmost overlay is placed
                   ;; into its own screen window. Focus/cursor/scroll track the active layer.
                   base-tree      (engine/strip-overlays full-tree)
                   overlay        (peek (engine/collect-overlays full-tree))
                   {:keys [rows cols]} (term/t-size terminal)
                   {:keys [min-width min-height]} (root-min base-tree)
                   too-small?     (or (< cols min-width) (< rows min-height))
                   screen         {:x 0 :y 0 :w cols :h rows}
                   base-placed    (when-not too-small?
                                    (inject-scroll app (engine/place base-tree screen)))
                   overlay-placed (when (and (not too-small?) overlay)
                                    (inject-scroll app (engine/place overlay (engine/overlay-window-rect overlay screen))))
                   active-placed  (or overlay-placed base-placed)
                   buf            (if too-small?
                                    (too-small-buffer rows cols min-width min-height)
                                    (cond-> (engine/render-buffer base-placed rows cols)
                                      overlay-placed (engine/paint overlay-placed screen)))
                   last-size      (::last-size rt)
                   size           {:rows rows :cols cols}
                   resized?       (boolean (and last-size (not= last-size size)))
                   prev           (when-not resized? (::prev-buffer rt))
                   ;; On a resize the whole screen is repainted from scratch (`:clear?`): a plain full
                   ;; repaint only writes non-blank cells, so without clearing, stale content from the
                   ;; old size would linger on screen.
                   ansi           (engine/frame->ansi prev buf {:sync?  (term/t-sync-supported? terminal)
                                                                :clear? resized?})]
               (term/t-write! terminal ansi)
               ;; Position the cursor AFTER the frame's drawing and flush once, so the cursor-move is
               ;; the last terminal command of the frame (otherwise the diff's writes leave the hardware
               ;; cursor wherever drawing ended, making the visible caret lag a frame on a real terminal).
               (when active-placed
                 (position-cursor! app terminal active-placed))
               (when (and too-small? (nil? active-placed))
                 (term/t-set-cursor! terminal 0 0 false))
               (term/t-flush! terminal)
               (swap! (runtime-atom-key app) assoc
                      ::prev-buffer buf
                      ::placed active-placed
                      ::last-size size)
               app)))))

;; ============================================================================
;; Step (single deterministic iteration)
;; ============================================================================

(>defn- placed-tree-of
        "Returns the placed tree most recently painted for `app` (from the runtime atom), or `nil`."
        [app]
        [any? => any?]
        (::placed (runtime app)))

(>defn step!
       "Runs one deterministic driver iteration for `app`: dispatches `key-event` and then repaints
   (`render!`). Returns `app`. Used by tests and the input loop.

   Dispatch order:
     1. Viewport scroll keys (`viewport-scroll-key!`): if the focused node lies inside a viewport and
        `key-event` is PageUp/PageDown (or `:up`/`:down` on a non-input focus), the enclosing viewport
        is scrolled and the focus/input pipeline is skipped.
     2. Otherwise `engine/process-key!` handles focus/typing/activation/global-keymap.
     3. `follow-focus!` then adjusts viewport scroll so the (possibly newly) focused node stays visible.

   The placed tree from the previous frame is used to locate the enclosing viewport for steps 1 & 3."
       ([app key-event]
        [any? map? => any?]
        (step! app key-event nil))
       ([app key-event global-keymap]
        [any? map? (? map?) => any?]
        (let [placed (placed-tree-of app)]
          (if (and placed (viewport-scroll-key! app placed key-event))
            (render! app)
            (do
              (engine/process-key! app key-event global-keymap)
              (follow-focus! app (or (placed-tree-of app) placed))
              (render! app))))
        app))

;; ============================================================================
;; Attach / mount / run / quit
;; ============================================================================

(>defn- initial-node-tree
        "Returns the current pure TUI node tree for `app` (root class + `db->tree`), for computing the
   initial focus. Returns `nil` if the app has no state/root yet."
        [app]
        [any? => any?]
        (engine/current-node-tree app))

(>defn attach!
       "Attaches `terminal` to `app` and performs the initial paint. Stashes the terminal in the runtime
   atom, enters the terminal (`term/t-enter!`), registers a resize handler that repaints on a terminal
   size change (`t-on-resize!` → `render!`), sets initial focus to the first node in the current tree's
   `focus-order` when `::engine/focus` is unset, and renders once. Returns `app`. Starts no loop."
       [app terminal]
       [any? any? => any?]
       (swap! (runtime-atom-key app) assoc ::terminal terminal)
       (term/t-enter! terminal)
       ;; Repaint when the terminal is resized. `render!` reads the fresh size and full-repaints with a
       ;; clear, so the layout self-corrects without waiting for a keypress.
       (term/t-on-resize! terminal (fn [] (render! app)))
       (when (nil? (engine/current-focus app))
         ;; Initial focus comes from the active layer (a startup overlay if one is open, else the base
         ;; tree with closed modals stripped) so it never lands inside an inactive modal.
         (let [order (engine/focus-order (engine/focusables (engine/active-tree (initial-node-tree app))))]
           (when-let [first-id (:id (first order))]
             (engine/focus! app first-id))))
       (render! app)
       app)

(>defn mount!
       "Attaches a terminal to `app` and starts the keyboard input loop on a new thread. `opts`:

     * `:terminal` - the `Terminal` to drive (default `(term/jline-terminal)`; tests pass a
       `string-terminal`).

   Returns a handle map `{:app :terminal :thread :running?}` where `:running?` is an atom that, when
   set false, stops the loop. The loop reads keys with `term/t-read-key`; a `nil` (EOF) read or
   `:running?` becoming false terminates it; `term/t-leave!` is always called on exit."
       ([app]
        [any? => ::handle]
        (mount! app {}))
       ([app {:keys [terminal global-keymap]}]
        [any? map? => ::handle]
        (let [terminal (or terminal (term/jline-terminal))
              running? (atom true)]
          (attach! app terminal)
          (let [loop-fn (fn input-loop []
                          (try
                            (loop []
                              (when @running?
                                (when-let [k (term/t-read-key terminal)]
                                  (when @running?
                               ;; Global/reserved chords (e.g. quit) are dispatched at the loop
                               ;; level so they fire regardless of which node has focus; everything
                               ;; else goes through the focus/input pipeline via `step!`.
                                    (if-let [h (and global-keymap (get global-keymap (engine/key-chord k)))]
                                      (h app k)
                                      (step! app k))
                                    (recur)))))
                            (finally
                              (term/t-leave! terminal))))
                thread  (Thread. ^Runnable loop-fn "fulcro-tui-input-loop")
                handle  {:app app :terminal terminal :thread thread :running? running?}]
            (swap! (:com.fulcrologic.fulcro.application/runtime-atom app) assoc ::handle handle)
            (.start thread)
            handle))))

(>defn run-blocking!
       "Mounts `app` (see `mount!`) and blocks until the input loop's thread finishes (the terminal
   reaches EOF or the loop is stopped). `opts` are passed to `mount!`. Returns the handle. (Named
   `run-blocking!` rather than `run!` to avoid shadowing `clojure.core/run!`.)"
       ([app]
        [any? => ::handle]
        (run-blocking! app {}))
       ([app opts]
        [any? map? => ::handle]
        (let [{:keys [^Thread thread] :as handle} (mount! app opts)]
          (.join thread)
          handle)))

(>defn quit!
       "Stops the input loop for a `mount!`/`run!` `handle` (or, given an `app`, looks up its terminal):
   sets `:running?` false, best-effort interrupts the loop thread, and leaves the terminal. Returns
   `handle-or-app`."
       [handle-or-app]
       [any? => any?]
       (let [handle (cond
                      (:running? handle-or-app) handle-or-app
                      :else (some-> (:com.fulcrologic.fulcro.application/runtime-atom handle-or-app)
                                    deref ::handle))
             {:keys [^Thread thread running? terminal]} (or handle {:terminal (terminal handle-or-app)})]
         (when running? (reset! running? false))
         (when thread (.interrupt thread))
         (when terminal
           (try (term/t-leave! terminal) (catch Throwable _ nil)))
         handle-or-app))

;; ============================================================================
;; Application builder
;; ============================================================================

(>defn application
       "Returns a synchronous raw Fulcro app suitable for TUI rendering. Builds a `rapp/fulcro-app` with
   synchronous transactions (`stx/with-synchronous-transactions`), the given `:root-class`, optional
   `:remotes`, and render hooks wired so that every state-change repaints through `render!` (when a
   terminal has been attached). By DEFAULT the app's state is initialized from the root class's
   declared `:initial-state` (the idiomatic Fulcro pattern — initial state is declared on Root and
   composes down the UI tree). This mirrors `com.fulcrologic.fulcro.application/fulcro-app`'s
   `:initialize-state?`, which also defaults true (application.cljc:323). `opts`:

     * `:root-class`    - the (required) TUI root component class (built with `com.fulcrologic.fulcro.components/defsc`).
     * `:initial-state` - (default true) initialize app state from `root-class`'s `:initial-state`.
                          Pass `false` to skip (advanced: e.g. you intend to install state yourself).
     * `:remotes`       - optional Fulcro remotes map.
     * `:inspect?`      - DEBUG ONLY (JVM, not babashka). When truthy, attaches Fulcro Inspect so a
                          running standalone Inspect (Electron) app on localhost:8237 observes this
                          app's transactions / network / state. Defaults to the `tui.inspect` system
                          property (`-Dtui.inspect=true`). The shim
                          (`com.fulcrologic.fulcro.tui.inspect`) ships in the library (`src/main`) but
                          is loaded lazily via `requiring-resolve` only when this is truthy, so normal
                          and babashka runs never load it (and its devtools deps stay off the path).

   No terminal is attached here; attach one with `attach!`/`mount!`."
       [{:keys [root-class remotes inspect?]
         :or   {inspect? (= "true" (System/getProperty "tui.inspect"))}
         :as   opts}]
       [map? => any?]
       (let [app (stx/with-synchronous-transactions
                   (rapp/fulcro-app
                    (cond-> {:root-class        root-class
                         ;; core-render! is invoked by the central render! after every tx; route it
                         ;; to our terminal paint (a no-op until a terminal is attached).
                             :core-render!      (fn [app _opts] (render! app))
                             :optimized-render! (fn [_app _opts] true)
                             :render-root!      (constantly true)}
                      remotes (assoc :remotes remotes))))]
         ;; Default-on: initialize from the Root's declared :initial-state unless the caller
         ;; explicitly opts out with `:initial-state false`.
         (when (get opts :initial-state true)
           (rapp/initialize-state! app root-class))
         (when inspect?
           ((requiring-resolve 'com.fulcrologic.fulcro.tui.inspect/add-inspect!) app))
         app))

;; ============================================================================
;; Inspection helpers (tests / REPL)
;; ============================================================================

(>defn screen-of
       "Returns the `engine/screen` (vector of row strings) of the buffer most recently painted for `app`, or
   `nil` if nothing has been painted yet."
       [app]
       [any? => (? vector?)]
       (some-> (runtime app) ::prev-buffer engine/screen))

(>defn screen-styled-of
       "Returns the `engine/screen-styled` (vector of rows of styled cell maps) of the buffer most recently
   painted for `app`, or `nil` if nothing has been painted yet."
       [app]
       [any? => (? vector?)]
       (some-> (runtime app) ::prev-buffer engine/screen-styled))
