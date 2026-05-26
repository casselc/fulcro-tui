(ns com.fulcrologic.fulcro.tui.driver
  "The side-effecting *driver* for the TUI rendering target: it renders a Fulcro app to a terminal
   and runs the input loop.

   This is the edge that ties together the pure TUI pipeline in `com.fulcrologic.fulcro.tui` (layout,
   paint, diff, focus, input) with a concrete `com.fulcrologic.fulcro.tui.terminal/Terminal`. Use
   `application` to build a synchronous raw Fulcro app whose renders repaint the terminal, `mount!`
   (or `run!`) to attach a terminal and start the keyboard input loop, and `step!` to drive a single
   deterministic iteration (used by tests).

   State/runtime keys (single source of truth):
     * Focus & carets & scroll are owned by `com.fulcrologic.fulcro.tui` (see that ns).
     * The attached terminal and the bookkeeping for incremental painting live in the app
       RUNTIME-ATOM under this namespace's keys (see `::terminal`, `::prev-buffer`, `::placed`,
       `::last-size`).

   This is JVM/babashka only (plain `.clj`).

   Viewport scrolling: `tui/place` lays a viewport's single child out at its natural size into a
   virtual rect, and `tui/render-buffer` blits only the window `[scroll-x scroll-y w h]` of that
   virtual content into the viewport's `::rect`. Scroll offsets are stored in the state-map under
   `:com.fulcrologic.fulcro.tui/scroll` keyed by viewport id; `render!` injects them onto the placed
   tree (`inject-scroll`), `follow-focus!` advances them after focus changes to keep the focused node
   visible, and PageUp/PageDown page-scroll via `viewport-scroll-key!`. Up/Down arrows move focus
   item-to-item (in `tui/process-key!`) and `follow-focus!` autoscrolls to track the focused item."
  (:require
    [clojure.spec.alpha :as s]
    [com.fulcrologic.fulcro.algorithms.denormalize :as fdn]
    [com.fulcrologic.fulcro.algorithms.tx-processing.synchronous-tx-processing :as stx]
    [com.fulcrologic.fulcro.raw.application :as rapp]
    [com.fulcrologic.fulcro.raw.components :as rc]
    [com.fulcrologic.fulcro.tui :as tui]
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
  {:min-width  (long (or (tui/node-attr node-tree :min-width) 1))
   :min-height (long (or (tui/node-attr node-tree :min-height) 1))})

(>defn too-small-buffer
  "Returns a `rows`x`cols` cell buffer painted with a centered \"terminal too small\" message asking
   for at least `min-w` x `min-h`. Used when the terminal is smaller than the root's declared minimum."
  [rows cols min-w min-h]
  [nat-int? nat-int? int? int? => map?]
  (let [msg (str "terminal too small — need " min-w "x" min-h)
        node (tui/place
               (tui/text {} msg)
               {:x 0 :y 0 :w cols :h rows})]
    (tui/render-buffer node rows cols)))

(>defn- caret-screen-position
  "Returns `[x y visible?]` for the hardware cursor given the placed `focused-node` (or `nil`), its
   effective on-screen `rect` (or `nil` when the node has none / is scrolled out of view), and the
   caret index `caret`.

   For a single-line `:input`, the cursor is placed at the rect origin advanced by the display width of
   the value up to `caret`, clamped to lie within the rect, and visible.

   For a multiline `:input` (`tui/multiline-input?`), the value is wrapped to the rect's width, the
   caret's visual `[row col]` is computed (`tui/caret->rowcol`), and the cursor is placed at
   `rect-origin + (row - top-line, col)` where `top-line` is the input's injected internal scroll
   (`::tui/text-scroll`). If that visual row is scrolled out of the rect's `[0, h)` window the cursor
   is hidden.

   For any other focused node the cursor is placed (visible) at the rect origin. When `rect` is `nil`
   the cursor is hidden at the origin."
  [focused-node rect caret]
  [(? map?) (? map?) int? => ::caret-pos]
  (if rect
    (cond
      (tui/multiline-input? focused-node)
      (let [value     (str (tui/node-attr focused-node :value))
            width     (:w rect)
            top       (max 0 (long (or (::tui/text-scroll focused-node) 0)))
            [row col] (tui/caret->rowcol value width caret)
            vy        (- row top)
            visible?  (and (>= vy 0) (< vy (:h rect)))
            max-x     (max (:x rect) (+ (:x rect) (dec (:w rect))))
            x         (min max-x (+ (:x rect) col))
            y         (+ (:y rect) vy)]
        [x y visible?])

      (= :input (::tui/tag focused-node))
      (let [value   (str (tui/node-attr focused-node :value))
            c       (max 0 (min caret (count value)))
            advance (tui/string-width (subs value 0 c))
            max-x   (max (:x rect) (+ (:x rect) (dec (:w rect))))
            x       (min max-x (+ (:x rect) advance))
            y       (:y rect)]
        [x y true])

      :else [(:x rect) (:y rect) true])
    [0 0 false]))

(>defn- focused-screen-rect
  "Returns the on-screen `::tui/rect` for the focused node `focus-id` within `placed`, accounting for an
   enclosing scrolled viewport, or `nil` when the focused node is scrolled out of its viewport's window.
   For a node NOT inside a viewport, returns the node's own placed `::tui/rect` (its absolute rect).
   For a node inside a viewport, its on-screen rect is `viewport-content-origin + (virtual-origin -
   scroll)`; if that lands outside the viewport's content window, `nil` is returned (cursor hidden)."
  [app placed focus-id]
  [any? any? any? => (? map?)]
  (if-let [{:keys [viewport virtual-rect]} (tui/focus-viewport-context placed focus-id)]
    (let [vp-id   (tui/node-attr viewport :id)
          scroll  (if (some? vp-id) (tui/viewport-scroll app vp-id) (or (::tui/scroll viewport) {:x 0 :y 0}))
          vsize   (::tui/virtual-size viewport)
          cv      (tui/content-view-size viewport)
          scroll  (tui/clamp-scroll scroll vsize cv)
          vp-rect (::tui/rect viewport)
          e       (+ (long (or (:padding (::tui/attrs viewport)) 0)) (if (:border? (::tui/attrs viewport)) 1 0))
          ox      (+ (:x vp-rect) e)
          oy      (+ (:y vp-rect) e)
          sx      (+ ox (- (:x virtual-rect) (:x scroll)))
          sy      (+ oy (- (:y virtual-rect) (:y scroll)))]
      (when (and (>= sy oy) (< sy (+ oy (:h cv)))
              (>= sx ox) (< sx (+ ox (:w cv))))
        {:x sx :y sy :w (:w virtual-rect) :h (:h virtual-rect)}))
    (some-> (tui/find-by-id placed focus-id) ::tui/rect)))

(>defn- position-cursor!
  "Positions the hardware cursor of `terminal` for `app` against the `placed` tree: finds the focused
   node (`tui/current-focus`), computes its on-screen rect (via `focused-screen-rect`, which accounts
   for a scrolled enclosing viewport and hides the cursor when the focused node is scrolled out of
   view), computes its caret position, and calls `term/t-set-cursor!`. Returns `app`."
  [app terminal placed]
  [any? any? any? => any?]
  (let [focus-id     (tui/current-focus app)
        focused-node (when (some? focus-id) (tui/find-by-id placed focus-id))
        screen-rect  (when (some? focus-id) (focused-screen-rect app placed focus-id))
        value        (str (tui/node-attr focused-node :value))
        caret        (if (some? focus-id) (tui/get-caret app focus-id (count value)) 0)
        [x y vis?]   (caret-screen-position focused-node screen-rect caret)]
    (term/t-set-cursor! terminal x y vis?)
    app))

(>defn- inject-scroll
  "Returns the placed `tree` with scroll state injected for the next paint, and records each multiline
   input's effective wrap width on `app`'s runtime (`tui/set-input-width!`).

   For every `:viewport` node it sets `::tui/scroll` from `app`'s scroll state (the state-map key
   `::tui/scroll`, keyed by viewport id), clamped via `tui/clamp-scroll` against the viewport's
   `::tui/virtual-size` and its content-area view size. Viewports without an `:id` keep their default
   `{:x 0 :y 0}`.

   For every multiline `:input` node (`tui/multiline-input?`) it records the input's content width
   (its placed content-area width) so key handling wraps at the painted width, then computes the
   internal top visual-line offset (`tui/text-scroll-top`) from the input's caret so the caret row
   stays visible, and assocs it under `::tui/text-scroll`.

   Walks the placed tree (including nested viewport content)."
  [app tree]
  [any? any? => any?]
  (letfn [(walk [x]
            (if (tui/node? x)
              (let [x (cond
                        (tui/viewport? x)
                        (let [id     (tui/node-attr x :id)
                              scroll (if (some? id) (tui/viewport-scroll app id) (::tui/scroll x))
                              scroll (tui/clamp-scroll scroll (::tui/virtual-size x) (tui/content-view-size x))]
                          (assoc x ::tui/scroll scroll))

                        (tui/multiline-input? x)
                        (let [id    (tui/node-attr x :id)
                              width (:w (tui/content-view-size x))
                              value (str (tui/node-attr x :value))
                              h     (:h (tui/content-view-size x))
                              caret (if (some? id) (tui/get-caret app id (count value)) (count value))]
                          (when (some? id) (tui/set-input-width! app id width))
                          (assoc x ::tui/text-scroll (tui/text-scroll-top value width caret h)))

                        :else x)
                    x (update x ::tui/children (fn [cs] (mapv walk cs)))]
                (if-let [vc (::tui/viewport-content x)]
                  (assoc x ::tui/viewport-content (walk vc))
                  x))
              x))]
    (walk tree)))

(>defn follow-focus!
  "Adjusts viewport scroll state so the currently focused node stays visible, then returns `app`.
   Using the `placed` tree, finds the focused node's enclosing viewport (`tui/focus-viewport-context`)
   and the focused node's VIRTUAL rect. Computes the minimal scroll that brings that rect into the
   viewport's content window (`tui/scroll-to-show`), clamps it (`tui/clamp-scroll`), and writes it to
   the viewport's scroll state (`tui/set-viewport-scroll!`) when it differs. A no-op when the focused
   node is not inside a viewport or the enclosing viewport has no `:id`."
  [app placed]
  [any? any? => any?]
  (let [focus-id (tui/current-focus app)]
    (when (some? focus-id)
      (when-let [{:keys [viewport virtual-rect]} (tui/focus-viewport-context placed focus-id)]
        (when-let [vp-id (tui/node-attr viewport :id)]
          (let [vsize     (::tui/virtual-size viewport)
                cr        (tui/content-view-size viewport)
                current   (tui/viewport-scroll app vp-id)
                desired   (tui/clamp-scroll (tui/scroll-to-show current virtual-rect cr) vsize cr)]
            (when (not= current desired)
              (tui/set-viewport-scroll! app vp-id desired))))))
    app))

(>defn- viewport-scroll-key!
  "If `key-event` is `:page-up`/`:page-down` and the currently focused node lies inside a viewport,
   scrolls that viewport's scroll state by a page and returns truthy (`:handled`). Returns `nil`
   (unhandled) otherwise, so the caller falls through to the normal key pipeline. `placed` is the
   current placed tree (for locating the enclosing viewport).

   `:up`/`:down` are NOT handled here: they drive focus navigation in `tui/process-key!` (moving
   focus item-to-item through the focus ring), and `follow-focus!` keeps the focused item visible —
   so arrowing through a focusable list autoscrolls its viewport. PageUp/PageDown remain the explicit
   page-scroll for a focused viewport."
  [app placed key-event]
  [any? any? map? => any?]
  (let [k        (:key key-event)
        focus-id (tui/current-focus app)
        ctx      (when (some? focus-id) (tui/focus-viewport-context placed focus-id))
        viewport (:viewport ctx)
        vp-id    (when viewport (tui/node-attr viewport :id))]
    (when (and viewport (some? vp-id))
      (let [vsize  (::tui/virtual-size viewport)
            cr     (tui/content-view-size viewport)
            view-h (:h cr)
            page   (max 1 (dec view-h))
            cur    (tui/viewport-scroll app vp-id)
            dy     (cond
                     (= k :page-down) page
                     (= k :page-up)   (- page)
                     :else            nil)]
        (when dy
          (let [next (tui/clamp-scroll (update cur :y + dy) vsize cr)]
            (tui/set-viewport-scroll! app vp-id next)
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
      (let [state-map  (some-> app state-atom-key deref)
            root-class (root-class-key rt)
            query      (rc/get-query root-class state-map)
            tree       (fdn/db->tree query state-map state-map)
            node-tree  (binding [tui/*app*           app
                                 tui/*current-focus* (tui/current-focus app)]
                         (tui/render-root root-class tree app))
            {:keys [rows cols]} (term/t-size terminal)
            {:keys [min-width min-height]} (root-min node-tree)
            too-small? (or (< cols min-width) (< rows min-height))
            placed     (when-not too-small?
                         (inject-scroll app (tui/place node-tree {:x 0 :y 0 :w cols :h rows})))
            buf        (if too-small?
                         (too-small-buffer rows cols min-width min-height)
                         (tui/render-buffer placed rows cols))
            last-size  (::last-size rt)
            size       {:rows rows :cols cols}
            resized?   (and last-size (not= last-size size))
            prev       (when-not resized? (::prev-buffer rt))
            ansi       (tui/frame->ansi prev buf {:sync? (term/t-sync-supported? terminal)})]
        (term/t-write! terminal ansi)
        ;; Position the cursor AFTER the frame's drawing and flush once, so the cursor-move is the
        ;; last terminal command of the frame (otherwise the diff's writes leave the hardware cursor
        ;; wherever drawing ended, making the visible caret lag a frame on a real terminal).
        (when placed
          (position-cursor! app terminal placed))
        (when (and too-small? (nil? placed))
          (term/t-set-cursor! terminal 0 0 false))
        (term/t-flush! terminal)
        (swap! (runtime-atom-key app) assoc
          ::prev-buffer buf
          ::placed placed
          ::last-size size)
        app))))

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
     2. Otherwise `tui/process-key!` handles focus/typing/activation/global-keymap.
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
         (tui/process-key! app key-event global-keymap)
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
  (tui/current-node-tree app))

(>defn attach!
  "Attaches `terminal` to `app` and performs the initial paint. Stashes the terminal in the runtime
   atom, enters the terminal (`term/t-enter!`), sets initial focus to the first node in the current
   tree's `focus-order` when `::tui/focus` is unset, and renders once. Returns `app`. Starts no loop."
  [app terminal]
  [any? any? => any?]
  (swap! (runtime-atom-key app) assoc ::terminal terminal)
  (term/t-enter! terminal)
  (when (nil? (tui/current-focus app))
    (let [order (tui/focus-order (tui/focusables (initial-node-tree app)))]
      (when-let [first-id (:id (first order))]
        (tui/focus! app first-id))))
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
                               (if-let [h (and global-keymap (get global-keymap (tui/key-chord k)))]
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
   terminal has been attached). When `:initial-state` is provided the app's state is initialized from
   the root class. `opts`:

     * `:root-class`    - the (required) TUI root component class (built with `tui/defsc`).
     * `:initial-state` - when truthy, initialize app state from `root-class`.
     * `:remotes`       - optional Fulcro remotes map.

   No terminal is attached here; attach one with `attach!`/`mount!`."
  [{:keys [root-class initial-state remotes]}]
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
    (when initial-state
      (rapp/initialize-state! app root-class))
    app))

;; ============================================================================
;; Inspection helpers (tests / REPL)
;; ============================================================================

(>defn screen-of
  "Returns the `tui/screen` (vector of row strings) of the buffer most recently painted for `app`, or
   `nil` if nothing has been painted yet."
  [app]
  [any? => (? vector?)]
  (some-> (runtime app) ::prev-buffer tui/screen))

(>defn screen-styled-of
  "Returns the `tui/screen-styled` (vector of rows of styled cell maps) of the buffer most recently
   painted for `app`, or `nil` if nothing has been painted yet."
  [app]
  [any? => (? vector?)]
  (some-> (runtime app) ::prev-buffer tui/screen-styled))
