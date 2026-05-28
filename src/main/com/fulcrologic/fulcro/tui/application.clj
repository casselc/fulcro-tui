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
    [com.fulcrologic.fulcro.tui.elements :as elements]
    [com.fulcrologic.fulcro.tui.engine :as engine]
    [com.fulcrologic.fulcro.tui.perf :as perf :refer [p]]
    [com.fulcrologic.fulcro.tui.terminal :as term]
    [com.fulcrologic.guardrails.core :refer [=> >def >defn >defn- ?]]))

;; ============================================================================
;; Runtime keys
;; ============================================================================

(>def ::terminal (? any?))
(>def ::prev-buffer (? map?))
(>def ::placed (? map?))
(>def ::last-size (? map?))
(>def ::handle map?)
(>def ::global-keymap (? map?))
(>def ::render-throttle-ms int?)
(>def ::last-render-ns (? any?))                            ; atom holding the last render time in ns
(>def ::render-scheduled? (? any?))                         ; atom<boolean> guarding a single trailing render
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
  (let [msg  (str "terminal too small — need " min-w "x" min-h)
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
      (let [value    (str (engine/node-attr focused-node :value))
            width    (:w rect)
            top      (max 0 (long (or (::engine/text-scroll focused-node) 0)))
            [row col] (engine/caret->rowcol value width caret)
            vy       (- row top)
            visible? (and (>= vy 0) (< vy (:h rect)))
            max-x    (max (:x rect) (+ (:x rect) (dec (:w rect))))
            x        (min max-x (+ (:x rect) col))
            y        (+ (:y rect) vy)]
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
        [x y vis?] (caret-screen-position focused-node screen-rect caret)]
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
  (p ::inject-scroll
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
      (walk tree))))

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
          (let [vsize   (::engine/virtual-size viewport)
                cr      (engine/content-view-size viewport)
                current (engine/viewport-scroll app vp-id)
                desired (engine/clamp-scroll (engine/scroll-to-show current virtual-rect cr) vsize cr)]
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
                     (= k :page-up) (- page)
                     :else nil)]
        (when dy
          (let [next (engine/clamp-scroll (update cur :y + dy) vsize cr)]
            (engine/set-viewport-scroll! app vp-id next)
            :handled))))))

(defn record-error!
  "Records `t` as `app`'s most-recent input/render-loop error, in the runtime atom under `::last-error`
   (with a monotonically increasing `::error-count`). This is how the loop tolerates an exception
   WITHOUT killing the session or corrupting the terminal: the error is captured for later/sane
   reporting (e.g. shown in-UI via `last-error`, or printed after the loop exits) instead of bubbling
   out of the thread. Returns `app`."
  [app t]
  (swap! (runtime-atom-key app)
    (fn [rt] (-> rt (assoc ::last-error t) (update ::error-count (fnil inc 0)))))
  app)

(defn last-error
  "Returns the most recent `Throwable` recorded by the input/render loop for `app` (or `nil`).
   `(get @(runtime-atom app) ::error-count)` holds how many have occurred. See `record-error!`."
  [app]
  (::last-error (runtime app)))

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
      (p `render! (locking terminal
        (let [;; `engine/current-node-tree` computes the pure node tree from state (root class +
              ;; `db->tree` + `render-root`, with the focus var bound) AND memoizes it in the runtime
              ;; atom keyed on state-map identity. Sharing it with `process-key!` means a keystroke
              ;; that only moves focus does not build the whole tree twice (once to resolve the focus
              ;; ring, once to paint) — the second consumer reuses the first's tree.
              full-tree      (p `render-root (engine/current-node-tree app))
              ;; Overlays (open `:modal` nodes) are composited on top of the base UI; the base is
              ;; laid out from the tree with all modals stripped, and the topmost overlay is placed
              ;; into its own screen window. Focus/cursor/scroll track the active layer.
              base-tree      (engine/strip-overlays full-tree)
              overlay        (peek (engine/collect-overlays full-tree))
              {:keys [rows cols]} (term/t-size terminal)
              {:keys [min-width min-height]} (root-min base-tree)
              too-small?     (or (< cols min-width) (< rows min-height))
              screen         {:x 0 :y 0 :w cols :h rows}
              base-placed    (p `layout
                               (when-not too-small?
                                 (inject-scroll app (engine/place base-tree screen))))
              overlay-placed (p `layout
                               (when (and (not too-small?) overlay)
                                 (inject-scroll app (engine/place overlay (engine/overlay-window-rect overlay screen)))))
              active-placed  (or overlay-placed base-placed)
              buf            (p `paint
                               ;; `*enhanced-keys?*` gates mnemonic-underline rendering: hints are
                               ;; painted only when the enhanced keyboard protocol is active.
                               (binding [engine/*enhanced-keys?* (term/t-enhanced-keys? terminal)]
                                 (if too-small?
                                   (too-small-buffer rows cols min-width min-height)
                                   (cond-> (engine/render-buffer base-placed rows cols)
                                     overlay-placed (engine/paint overlay-placed screen)))))
              last-size      (::last-size rt)
              size           {:rows rows :cols cols}
              resized?       (boolean (and last-size (not= last-size size)))
              ;; A `redraw!` request (e.g. Ctrl-L) forces a clear + full repaint to recover a screen
              ;; corrupted by stray output (a rogue log line, another process writing to the tty…).
              force-redraw?  (boolean (::force-redraw? rt))
              full-repaint?  (or resized? force-redraw?)
              prev           (when-not full-repaint? (::prev-buffer rt))
              ;; On a resize/forced redraw the whole screen is repainted from scratch (`:clear?`): a
              ;; plain full repaint only writes non-blank cells, so without clearing, stale content
              ;; (from the old size, or stray output) would linger on screen.
              ansi           (p `serialize
                               (engine/frame->ansi prev buf {:sync?  (term/t-sync-supported? terminal)
                                                             :clear? full-repaint?}))]
          (p `write (term/t-write! terminal ansi))
          ;; Position the cursor AFTER the frame's drawing and flush once, so the cursor-move is
          ;; the last terminal command of the frame (otherwise the diff's writes leave the hardware
          ;; cursor wherever drawing ended, making the visible caret lag a frame on a real terminal).
          (p `cursor
            (when active-placed
              (position-cursor! app terminal active-placed))
            (when (and too-small? (nil? active-placed))
              (term/t-set-cursor! terminal 0 0 false)))
          (p `flush (term/t-flush! terminal))
          (swap! (runtime-atom-key app) assoc
            ::prev-buffer buf
            ::placed active-placed
            ::last-size size
            ::force-redraw? false)
          app))))))

(>defn request-render!
  "Requests a repaint of `app`, coalescing bursts into at most one render per throttle window.

The throttle interval (ms) is read from the runtime atom (`::render-throttle-ms`, default `0`). When
the interval is `<= 0` (the default for `attach!`/`step!`/direct `mount!` — i.e. the deterministic
test path) this renders SYNCHRONOUSLY and is identical to calling `render!`.

When the interval is `> 0` (the live run path: `run-blocking!`/`start!`) it is a leading+trailing
edge throttle. If at least the interval has elapsed since the last render it renders immediately
(leading edge) and records the time. Otherwise it schedules ONE daemon thread that sleeps the
remaining time and then renders the LATEST app state (trailing edge); further requests inside that
window coalesce into that single scheduled render (it reads current state at paint time, so the
final state is always painted). The deferred paint is wrapped in try/catch so a render that lands
after `quit!` closes the terminal cannot crash the daemon. Returns `app`."
  [app]
  [any? => any?]
  (let [rt        (runtime app)
        throttle  (long (or (::render-throttle-ms rt) 0))
        last-atom (::last-render-ns rt)
        sched     (::render-scheduled? rt)]
    (if (or (<= throttle 0) (nil? last-atom) (nil? sched))
      (render! app)
      (let [interval-ns (* throttle 1000000)
            now         (System/nanoTime)
            last        (long (or @last-atom 0))
            elapsed     (- now last)]
        (if (>= elapsed interval-ns)
          (do
            (reset! last-atom now)
            (render! app))
          (when (compare-and-set! sched false true)
            (let [remaining-ms (max 1 (quot (- interval-ns elapsed) 1000000))
                  runnable     (fn deferred-render []
                                 (try
                                   (Thread/sleep (long remaining-ms))
                                   (reset! last-atom (System/nanoTime))
                                   (reset! sched false)
                                   ;; Late paint guard: after quit! the loop's `:running?` is false
                                   ;; and the terminal is closed. Skip if shutting down, and wrap the
                                   ;; render so a stale frame can never crash the daemon nor surface.
                                   (let [running? (some-> (runtime app) ::handle :running? deref)]
                                     (when (not (false? running?))
                                       (try (render! app) (catch Throwable t (record-error! app t)))))
                                   (catch Throwable t
                                     (reset! sched false)
                                     (record-error! app t))))
                  t            (Thread. ^Runnable runnable "fulcro-tui-render")]
              (.setDaemon t true)
              (.start t))))))
    app))

(>defn redraw!
  "Forces a full repaint of `app` on the next frame — clears the screen and repaints from scratch,
   discarding the diff baseline — to recover a terminal corrupted by stray output (a rogue log line,
   another process writing to the tty, etc.). Wire it to a chord such as Ctrl-L via `:global-keymap`:
   `{[:ctrl \"l\"] (fn [app _] (redraw! app))}`. Returns `app`."
  [app]
  [any? => any?]
  (swap! (runtime-atom-key app) assoc ::force-redraw? true)
  (request-render! app)
  app)

;; ============================================================================
;; Step (single deterministic iteration)
;; ============================================================================

(>defn- placed-tree-of
  "Returns the placed tree most recently painted for `app` (from the runtime atom), or `nil`."
  [app]
  [any? => any?]
  (::placed (runtime app)))

(>defn- dispatch-key!
  "Dispatches `key-event` against `app` WITHOUT rendering, returning `app`. This is the state-mutating
half of `step!` (the render is the caller's responsibility, so the live loop can use a throttled
`request-render!` while `step!`/tests render synchronously).

Dispatch order:
1. Viewport scroll keys (`viewport-scroll-key!`): if the focused node lies inside a viewport and
   `key-event` is PageUp/PageDown, the enclosing viewport is scrolled and the focus/input pipeline
   is skipped.
2. Otherwise `engine/process-key!` handles focus/typing/activation/global-keymap, then
   `follow-focus!` adjusts viewport scroll so the (possibly newly) focused node stays visible.

The placed tree from the previous frame is used to locate the enclosing viewport for steps 1 & 2."
  [app key-event global-keymap]
  [any? map? (? map?) => any?]
  ;; Batch this keystroke's renders: `focus!` and any transaction fired here would each trigger an
  ;; immediate synchronous render, so a single arrow key (which moves focus AND then scrolls the
  ;; viewport via `follow-focus!`) would paint twice. Suppress those eager renders; the caller
  ;; (`step!`/the input loop) renders ONCE after dispatch returns.
  (binding [engine/*suppress-render* true]
    (let [placed (placed-tree-of app)]
      (when-not (and placed (viewport-scroll-key! app placed key-event))
        (engine/process-key! app key-event global-keymap)
        (follow-focus! app (or (placed-tree-of app) placed)))))
  app)

(>defn step!
  "Runs one deterministic driver iteration for `app`: dispatches `key-event` (`dispatch-key!`) and
then repaints SYNCHRONOUSLY (`render!`). Returns `app`. Used by tests (which read the screen right
after `step!` returns) and historically by the input loop. The live input loop now dispatches and
then requests a THROTTLED render (`request-render!`) instead of calling `step!`, so this stays
synchronous for deterministic tests.

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
   (dispatch-key! app key-event global-keymap)
   (render! app)
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
  ;; clear, so the layout self-corrects without waiting for a keypress. Routed through
  ;; `request-render!` so rapid resize bursts coalesce on the live path; with throttling disabled
  ;; (tests/direct attach!) this is a synchronous `render!`.
  (term/t-on-resize! terminal (fn [] (request-render! app)))
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

* `:terminal`      - the `Terminal` to drive (default `(term/jline-terminal)`; tests pass a
                     `string-terminal`).
* `:global-keymap` - optional map of `key-chord` -> `(fn [app key-event])`. These reserved
                     chords (e.g. a quit chord) are dispatched at the loop level so they fire
                     regardless of which node has focus; everything else goes through the
                     focus/input pipeline (`step!`). Defaults to the keymap registered on the
                     app at `application`/`start!` time (`::global-keymap`), if any.
* `:on-error`      - optional `(fn [app throwable])` called for an exception raised while handling a
                     keystroke (a global-keymap handler, key dispatch, or a synchronous render). The
                     loop TOLERATES these: it records the error (see `last-error`/`record-error!`),
                     invokes `:on-error`, attempts a repaint, and KEEPS RUNNING — one bad keystroke
                     does not end the session. (A throwable escaping the loop structure itself, e.g.
                     from reading keys, still ends the loop, is stashed on the handle's `:error` atom,
                     and leaves the terminal.)
* `:max-fps`       - optional max live render frequency (frames/sec). When present and positive the
                     input loop and resize/render hooks coalesce repaints to at most one per
                     `(quot 1000 max-fps)` ms (trailing-edge debounce via `request-render!`). When
                     absent (the default for a directly-called `mount!`, e.g. tests) throttling is
                     DISABLED and every render path is synchronous == `step!`. `run-blocking!`/
                     `start!` default this to 15.

Returns a handle map `{:app :terminal :thread :running? :error}`. `:running?` is an atom that, when
set false, stops the loop; `:error` is an atom holding any uncaught loop exception (else `nil`).
The loop reads keys with `term/t-read-key`; a `nil` (EOF) read or `:running?` becoming false
terminates it; `term/t-leave!` is always called on exit (in a `finally`)."
  ([app]
   [any? => ::handle]
   (mount! app {}))
  ([app {:keys [terminal global-keymap on-error max-fps]}]
   [any? map? => ::handle]
   (let [terminal      (or terminal (term/jline-terminal))
         global-keymap (or global-keymap (::global-keymap (runtime app)))
         throttle-ms   (if (and max-fps (pos? (long max-fps))) (quot 1000 (long max-fps)) 0)
         running?      (atom true)
         error         (atom nil)]
     ;; Install throttle bookkeeping BEFORE attach! so the initial paint + resize handler see it.
     ;; With throttle-ms 0 (no :max-fps) request-render! is synchronous == today.
     (swap! (runtime-atom-key app) assoc
       ::render-throttle-ms throttle-ms
       ::last-render-ns (atom 0)
       ::render-scheduled? (atom false))
     (attach! app terminal)
     (let [loop-fn (fn input-loop []
                     (try
                       (loop []
                         (when @running?
                           (when-let [k (term/t-read-key terminal)]
                             (when @running?
                               ;; Global/reserved chords (e.g. quit) are dispatched at the loop
                               ;; level so they fire regardless of which node has focus; everything
                               ;; else goes through the focus/input pipeline. We dispatch the key
                               ;; (state mutation) then request a THROTTLED render — coalescing the
                               ;; loop's request with the synchronous `:core-render!` hook that a
                               ;; transaction in `dispatch-key!` may also fire, killing the double
                               ;; render and capping live repaints at `:max-fps`.
                               ;; Tolerate per-keystroke errors: a throwing handler / dispatch /
                               ;; synchronous render must NOT kill the whole session. Record the
                               ;; error (for sane reporting via `last-error`), hand it to `:on-error`
                               ;; if supplied, attempt a repaint so the UI recovers, and keep looping.
                               (try
                                 (if-let [h (and global-keymap (get global-keymap (engine/key-chord k)))]
                                   (h app k)
                                   (do
                                     ;; nil global-keymap here mirrors the prior `(step! app k)` call;
                                     ;; reserved chords are already handled by the branch above.
                                     ;; `*enhanced-keys?*` gates control-shortcut dispatch in
                                     ;; `engine/process-key!` to terminals that support the protocol.
                                     (binding [engine/*enhanced-keys?* (term/t-enhanced-keys? terminal)]
                                       (dispatch-key! app k nil))
                                     (request-render! app)))
                                 (catch Throwable t
                                   (record-error! app t)
                                   (when on-error (try (on-error app t) (catch Throwable _ nil)))
                                   (try (request-render! app) (catch Throwable _ nil))))
                               (recur)))))
                       ;; C2: an uncaught exception must NOT silently kill the thread and make
                       ;; `run-blocking!`'s join look like a clean exit. Stash it on the handle's
                       ;; `:error` atom and hand it to `:on-error` (if supplied) so callers can see it.
                       (catch Throwable t
                         (reset! error t)
                         (when on-error (try (on-error app t) (catch Throwable _ nil))))
                       (finally
                         (term/t-leave! terminal))))
           thread  (Thread. ^Runnable loop-fn "fulcro-tui-input-loop")
           handle  {:app app :terminal terminal :thread thread :running? running? :error error}]
       (swap! (:com.fulcrologic.fulcro.application/runtime-atom app) assoc ::handle handle)
       (.start thread)
       handle))))

(>defn run-blocking!
  "Mounts `app` (see `mount!`) and blocks until the input loop's thread finishes (the terminal
reaches EOF or the loop is stopped). `opts` are passed to `mount!`. Returns the handle. (Named
`run-blocking!` rather than `run!` to avoid shadowing `clojure.core/run!`.)

Unlike a bare `mount!`, the live run path DEFAULTS to `:max-fps 30` (≈33ms trailing-edge debounce)
so a real interactive session caps repaints; override with an explicit `:max-fps` in `opts` (use
`0`/negative to disable throttling).

When the `fulcro.tui.perf` system property is set (see `com.fulcrologic.fulcro.tui.perf`), the
whole session is profiled automatically and a self-time report is printed to stdout once the loop
ends — by then `mount!`'s `finally` has left/restored the terminal, so the table prints cleanly.
Without the property the `perf/profile` wrapper compiles away entirely (zero overhead)."
  ([app]
   [any? => ::handle]
   (run-blocking! app {}))
  ([app opts]
   [any? map? => ::handle]
   (perf/profile {}
     (let [opts                            (merge {:max-fps 30} opts)
           {:keys [^Thread thread] :as handle} (mount! app opts)]
       (.join thread)
       handle))))

(>defn quit!
  "Stops the input loop for a `mount!`/`run-blocking!` `handle` (or, given an `app`, looks up its
handle/terminal). Returns `handle-or-app`. Steps, in order:

1. Sets `:running?` false (so the loop won't process the next key).
2. Unregisters the terminal's resize handler (`t-on-resize!` with `nil`) — otherwise a stray
   SIGWINCH delivered after the terminal is closed would invoke `render!` against a closed
   terminal (C1).
3. Leaves the terminal (`t-leave!`). For a real JLine terminal this CLOSES the terminal, which
   forces a thread parked in the blocking `t-read-key` to return EOF — this (not the interrupt)
   is what actually unblocks and ends a programmatically-quit loop (C3).
4. Best-effort `.interrupt` of the loop thread as a fallback.

Residual limitation: unblocking the blocked read depends on JLine closing the input on `.close`;
if a transport does not, the loop ends on the next keypress/EOF instead."
  [handle-or-app]
  [any? => any?]
  (let [handle (cond
                 (:running? handle-or-app) handle-or-app
                 :else (some-> (:com.fulcrologic.fulcro.application/runtime-atom handle-or-app)
                         deref ::handle))
        {:keys [^Thread thread running? terminal]} (or handle {:terminal (terminal handle-or-app)})]
    (when running? (reset! running? false))
    (when terminal
      ;; C1: drop the resize handler BEFORE closing, so a concurrent SIGWINCH can't paint a
      ;; closed terminal. Then C3: t-leave! closes it, forcing the blocked read to EOF.
      (try (term/t-on-resize! terminal nil) (catch Throwable _ nil))
      (try (term/t-leave! terminal) (catch Throwable _ nil)))
    (when thread (.interrupt thread))
    handle-or-app))

;; ============================================================================
;; Application builder
;; ============================================================================

(>defn application
  "Returns a synchronous raw Fulcro app suitable for TUI rendering. Builds a `rapp/fulcro-app` with
synchronous transactions (`stx/with-synchronous-transactions`), the given `:root-class`,
and render hooks wired so that every state-change repaints through TUI rendering.

By DEFAULT the app's state is initialized from the root class's
declared `:initial-state` (the idiomatic Fulcro pattern — initial state is declared on Root and
composes down the UI tree).

Additional options from this library:

* `:global-keymap` - optional default `key-chord` -> `(fn [app key-event])` map registered on the
                     app; `mount!`/`run-blocking!`/`start!` use it unless they are passed their
                     own `:global-keymap`. Handy for a quit chord without repeating it per run.
* `:inspect?`      - DEBUG ONLY (JVM, not babashka). When truthy, attaches Fulcro Inspect so a
                     running standalone Inspect (Electron) app on localhost:8237 observes this
                     app's transactions / network / state. Defaults to the `tui.inspect` system
                     property (`-Dtui.inspect=true`). The shim
                     (`com.fulcrologic.fulcro.tui.inspect`) ships in the library (`src/main`) but
                     is loaded lazily via `requiring-resolve` only when this is truthy, so normal
                     and babashka runs never load it (and its devtools deps stay off the path).


See Fulcro's rapp/fulcro-app for additional options.

Typically you will use `run-blocking!` to actually run the application."
  [{:keys [root-class inspect? global-keymap]
    :or   {inspect? (= "true" (System/getProperty "tui.inspect"))}
    :as   opts}]
  [map? => any?]
  (let [app (stx/with-synchronous-transactions
              (rapp/fulcro-app
                (merge
                  opts
                  (cond-> {:core-render!      (fn [app _opts]
                                                ;; While the driver is batching a keystroke's
                                                ;; mutations (`engine/*suppress-render*`), a
                                                ;; transaction's render is deferred to the single
                                                ;; post-dispatch render so one key = one frame.
                                                (when-not engine/*suppress-render*
                                                  (request-render! app)))
                           :optimized-render! (fn [_app _opts] true)
                           :render-root!      (constantly true)}))))]
    ;; Default-on: initialize from the Root's declared :initial-state unless the caller
    ;; explicitly opts out with `:initial-state false`.
    (when (get opts :initial-state true)
      (rapp/initialize-state! app root-class))
    (when global-keymap
      (swap! (runtime-atom-key app) assoc ::global-keymap global-keymap))
    (when inspect?
      ((requiring-resolve 'com.fulcrologic.fulcro.tui.inspect/add-inspect!) app))
    app))

(>defn start!
  "Builds an `application` and runs it to completion in one call — the convenience entrypoint for
the common case. `(start! app-opts)` or `(start! app-opts run-opts)` is equivalent to
`(run-blocking! (application app-opts) run-opts)`. `app-opts` are `application`'s options
(`:root-class`, `:initial-state`, `:remotes`, `:global-keymap`, `:inspect?`); `run-opts` are
`mount!`'s (`:terminal`, `:global-keymap`, `:on-error`, `:max-fps`). Like `run-blocking!`, live
rendering DEFAULTS to `:max-fps 30`; override via `run-opts`. Blocks until the loop ends
(EOF/`quit!`). Returns the handle."
  ([app-opts]
   [map? => ::handle]
   (start! app-opts {}))
  ([app-opts run-opts]
   [map? map? => ::handle]
   (run-blocking! (application app-opts) run-opts)))

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
