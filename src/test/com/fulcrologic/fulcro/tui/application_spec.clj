(ns com.fulcrologic.fulcro.tui.application-spec
  (:require
    [clojure.string :as str]
    [com.fulcrologic.fulcro.components :as comp]
    [com.fulcrologic.fulcro.mutations :as m]
    [com.fulcrologic.fulcro.tui.application :as app]
    [com.fulcrologic.fulcro.tui.elements :as elements]
    [com.fulcrologic.fulcro.tui.engine :as engine]
    [com.fulcrologic.fulcro.tui.terminal :as term]
    [fulcro-spec.core :refer [=> assertions component specification]]))

;; ---------------------------------------------------------------------------
;; Test app: a vbox with two inputs (:a/:b) and a button (:ok).
;; ---------------------------------------------------------------------------

(m/defmutation set-a [{:keys [v]}]
  (action [{:keys [state]}] (swap! state assoc :app/a v)))

(m/defmutation set-b [{:keys [v]}]
  (action [{:keys [state]}] (swap! state assoc :app/b v)))

(m/defmutation set-ok [_]
  (action [{:keys [state]}] (swap! state assoc :app/ok? true)))

(comp/defsc Root [this {:keys [app/a app/b app/ok?]}]
  {:query         [:app/a :app/b :app/ok?]
   :ident         (fn [] [:component/id ::root])
   :initial-state {:app/a "" :app/b "" :app/ok? false}}
  (elements/vbox {:id "root"}
    (elements/input {:id        :a
                     :value     a
                     :on-change (fn [v _caret] (comp/transact! this [(set-a {:v v})]))})
    (elements/input {:id        :b
                     :value     b
                     :on-change (fn [v _caret] (comp/transact! this [(set-b {:v v})]))})
    (elements/button {:id          :ok
                      ;; route-key only fires :on-key, so activation lives there (enter/space).
                      :on-key      (fn [e]
                                     (when (#{:enter " "} (:key e))
                                       (comp/transact! this [(set-ok {})])
                                       :handled))
                      :on-activate (fn [] (comp/transact! this [(set-ok {})]))}
      (if ok? "DONE" "OK"))))

(defn- new-app []
  (app/application {:root-class Root :initial-state true}))

(defn- state [app]
  (deref (:com.fulcrologic.fulcro.application/state-atom app)))

(defn- render-loop-of [app]
  (:com.fulcrologic.fulcro.tui.application/render-loop
    (deref (:com.fulcrologic.fulcro.application/runtime-atom app))))

(defn- blocking-terminal
  "A fake `Terminal` whose `t-read-key` BLOCKS until `:release` is delivered (then returns nil = EOF), so
   the input loop stays parked and the dedicated render loop keeps running — letting a test exercise
   off-input-thread repaints and live-loop teardown WITHOUT the input loop EOF-ing first (the render loop
   must NOT outlive the terminal, so we can no longer lean on it surviving an immediate EOF). Returns
   `{:terminal t :release p :left? a}`; `t-leave!` sets `:left?`. `t-size` is fixed at `rows`x`cols`."
  ([] (blocking-terminal 10 30))
  ([rows cols]
   (let [release (promise)
         left?   (atom false)]
     {:terminal (reify term/Terminal
                  (t-size [_] {:rows rows :cols cols})
                  (t-read-key [_] @release)
                  (t-write! [_ _] nil)
                  (t-flush! [_] nil)
                  (t-set-cursor! [_ _ _ _] nil)
                  (t-enter! [_] nil)
                  (t-leave! [_] (reset! left? true) nil)
                  (t-sync-supported? [_] false)
                  (t-enhanced-keys? [_] false)
                  (t-on-resize! [_ _] nil))
      :release release
      :left?   left?})))

;; A root with a fixed-height viewport of many focusable items, for scrolling/follow-focus tests.
(comp/defsc VPRoot [this _props]
  {:query         [:vp/x]
   :ident         (fn [] [:component/id ::vproot])
   :initial-state {:vp/x 1}}
  (elements/vbox {:id "root"}
    (elements/viewport {:id :list :height 4}
      (elements/vbox {}
        (for [i (range 10)]
          (elements/button {:id (keyword (str "item-" i))} (str "Item " i)))))))

(defn- new-vp-app []
  (app/application {:root-class VPRoot :initial-state true}))

;; A root with a single fixed-size multiline text-area, for multiline editing/cursor/scroll tests.
(m/defmutation set-notes [{:keys [v]}]
  (action [{:keys [state]}] (swap! state assoc :ml/notes v)))

(comp/defsc MLRoot [this {:keys [ml/notes]}]
  {:query         [:ml/notes]
   :ident         (fn [] [:component/id ::mlroot])
   :initial-state {:ml/notes ""}}
  (elements/vbox {:id "root"}
    (elements/input {:id         :notes
                     :multiline? true
                     :height     3
                     :width      9
                     :value      notes
                     :on-change  (fn [v _caret] (comp/transact! this [(set-notes {:v v})]))})))

(defn- new-ml-app []
  (app/application {:root-class MLRoot :initial-state true}))

;; ---------------------------------------------------------------------------

(specification {:covers {`app/application      "bc5160,102e48"
                         `app/attach!          "4b0c66,71c9c3"
                         `app/render!          "b81026,8cfecc"
                         `app/screen-of        "0ca459,662c58"
                         `app/screen-styled-of "3815eb,5083b1"
                         `app/terminal         "18f70c,089ec1"}} "application / attach! (initial paint)"
  (let [app (new-app)
        t   (term/string-terminal {:rows 10 :cols 30})]
    (app/attach! app t)
    (let [scr (app/screen-of app)]
      (assertions
        "attach! enters the terminal"
        (:entered? @(.-state t)) => true
        "attach! stashes the terminal on the app runtime (terminal accessor)"
        (app/terminal app) => t
        "the initial paint puts the (empty) inputs on rows 0 and 1 (blank) and the button label on row 2"
        (nth scr 0) => "                              "
        (nth scr 1) => "                              "
        (nth scr 2) => "OK                            "
        "the screen has one row per terminal row"
        (count scr) => 10
        "screen-styled-of returns the styled cells of the most recent buffer (button label cells on row 2)"
        (mapv :ch (take 2 (nth (app/screen-styled-of app) 2))) => [\O \K]
        "initial focus is the first focusable (input :a)"
        (engine/current-focus app) => :a
        "the hardware cursor is shown at input :a's caret origin (0,0)"
        (term/cursor t) => {:x 0 :y 0 :visible? true}))))

(specification "terminal restore on crash — JVM shutdown hook"
  (let [hook-of (fn [app] (:com.fulcrologic.fulcro.tui.application/shutdown-hook
                            (deref (:com.fulcrologic.fulcro.application/runtime-atom app))))]
    (component "mount! installs a shutdown hook that leaves the terminal if the process dies before teardown"
      (let [app    (new-app)
            t      (term/string-terminal {:rows 10 :cols 30})
            handle (app/mount! app {:terminal t})
            hook   (hook-of app)]
        (assertions
          "mount! registered a JVM shutdown-hook Thread (stashed under ::shutdown-hook)"
          (instance? Thread hook) => true)
        ;; Isolate the hook's effect from the normal teardown: pretend the terminal was NOT yet left,
        ;; then run the hook directly (the JVM runs it on an abnormal exit) and confirm it leaves it.
        (swap! (.-state t) assoc :left? false)
        (.run ^Thread hook)
        (assertions
          "running the hook leaves the terminal (restores alt-screen / auto-wrap / cursor / raw mode)"
          (:left? @(.-state t)) => true)
        (app/quit! handle)))

    (component "mount! restores the terminal (and drops the hook) if attach! throws before the loop starts"
      ;; attach! enters the alt-screen (t-enter!) then does the first paint; if either throws, the
      ;; terminal must NOT be stranded in raw/alt-screen mode. The hook is installed BEFORE attach!, and
      ;; the catch leaves the terminal immediately + deregisters the hook, then rethrows.
      (let [app   (new-app)
            left? (atom false)
            t     (reify term/Terminal
                    (t-size [_] {:rows 10 :cols 30})
                    (t-read-key [_] nil)
                    (t-write! [_ _] nil)
                    (t-flush! [_] nil)
                    (t-set-cursor! [_ _ _ _] nil)
                    (t-enter! [_] (throw (ex-info "enter boom" {})))
                    (t-leave! [_] (reset! left? true))
                    (t-sync-supported? [_] false)
                    (t-enhanced-keys? [_] false)
                    (t-on-resize! [_ _] nil))]
        (assertions
          "mount! propagates the attach! failure to the caller"
          (try (app/mount! app {:terminal t}) ::no-throw (catch Throwable _ ::threw)) => ::threw
          "the terminal was left/restored despite the failure (not stranded in alt-screen)"
          @left? => true
          "the crash hook was deregistered after the failed mount (not leaked)"
          (hook-of app) => nil)))

    (component "re-mounting the same app REPLACES the hook rather than accumulating"
      ;; The no-accumulation invariant holds regardless of WHEN the previous hook is deregistered: with
      ;; this feature alone, install-terminal-restore-hook! drops the prior hook on the next mount!; with
      ;; the centralized shutdown! (fix/dirty-terminal-on-exit), quit! already dropped it. Either way,
      ;; after a mount!→quit!→mount! cycle the first hook is gone and the second is a distinct live hook —
      ;; so this test passes both on this branch in isolation and on the integration. (We deliberately do
      ;; NOT assert whether the hook survives the bare quit!, since that legitimately differs by config.)
      (let [app   (new-app)
            t1    (term/string-terminal {:rows 10 :cols 30})
            h1    (app/mount! app {:terminal t1})
            hook1 (hook-of app)
            _     (app/quit! h1)
            t2    (term/string-terminal {:rows 10 :cols 30})
            h2    (app/mount! app {:terminal t2})
            hook2 (hook-of app)]
        (app/quit! h2)
        (assertions
          "the first mount! registered a hook Thread"
          (instance? Thread hook1) => true
          "re-mounting installs a DIFFERENT live hook Thread"
          (instance? Thread hook2) => true
          (identical? hook1 hook2) => false
          "the previous hook is deregistered from the JVM (no accumulation; removeShutdownHook => false)"
          (.removeShutdownHook (Runtime/getRuntime) ^Thread hook1) => false)))

    (component "run-blocking! deregisters the hook after a clean session (no hook accumulation)"
      (let [app (new-app)
            t   (term/string-terminal {:rows 10 :cols 30 :keys [{:key "a" :char "a"}]})]
        (app/run-blocking! app {:terminal t :max-fps 120})
        (assertions
          "after a clean run-blocking! session the ::shutdown-hook entry is cleared"
          (hook-of app) => nil
          "the terminal was left normally"
          (:left? @(.-state t)) => true)))))

(specification {:covers {`app/step! "6b509b,7a63d8"}} "step! — focus, typing, and activation"
  (component "Tab moves focus and follows the cursor to the newly focused input"
    (let [app (new-app)
          t   (term/string-terminal {:rows 10 :cols 30})]
      (app/attach! app t)
      (app/step! app {:key :tab})
      (assertions
        "Tab advances focus from :a to :b"
        (engine/current-focus app) => :b
        "the cursor moves to input :b's row (row 1) at the caret origin"
        (term/cursor t) => {:x 0 :y 1 :visible? true})))

  (component "typing printable keys into the focused input updates state, screen, and caret"
    (let [app (new-app)
          t   (term/string-terminal {:rows 10 :cols 30})]
      (app/attach! app t)                                   ; focus :a
      (app/step! app {:key "H" :char "H"})
      (app/step! app {:key "i" :char "i"})
      (assertions
        "the input's :on-change transaction updated the value in app state"
        (:app/a (state app)) => "Hi"
        "the painted screen reflects the typed text on input :a's row"
        (nth (app/screen-of app) 0) => "Hi                            "
        "the caret advanced by the number of typed characters"
        (engine/get-caret app :a 0) => 2
        "the hardware cursor follows the caret (x=2 on row 0)"
        (term/cursor t) => {:x 2 :y 0 :visible? true})))

  (component "activating the button (Tab to it, then Enter) runs its handler"
    (let [app (new-app)
          t   (term/string-terminal {:rows 10 :cols 30})]
      (app/attach! app t)
      (app/step! app {:key :tab})                           ; a -> b
      (app/step! app {:key :tab})                           ; b -> ok
      (assertions
        "focus reaches the button"
        (engine/current-focus app) => :ok)
      (app/step! app {:key :enter})
      (assertions
        "the button's handler ran (its mutation set the flag in state)"
        (:app/ok? (state app)) => true
        "the repaint shows the button's updated label"
        (nth (app/screen-of app) 2) => "DONE                          "))))

(specification {:covers {`app/render!          "b81026,8cfecc"
                         `app/too-small-buffer "6bd38f,3e293f"}} "render! — diff path & resize"
  (component "a no-op re-render produces no terminal output"
    (let [app (new-app)
          t   (term/string-terminal {:rows 10 :cols 30})]
      (app/attach! app t)
      (let [before (term/output t)]
        (app/render! app)                                   ; identical frame
        (assertions
          "rendering an unchanged frame writes no additional bytes to the terminal"
          (term/output t) => before))))

  (component "a resize auto-repaints (no keypress) with a screen clear and the new dimensions"
    (let [app (new-app)
          t   (term/string-terminal {:rows 10 :cols 30})]
      (app/attach! app t)
      (let [before (term/output t)]
        ;; resize! delivers the terminal's resize signal; the registered handler
        ;; repaints immediately — no key needs to be pressed.
        (term/resize! t 5 20)
        (assertions
          "the resize handler wrote a (full) repaint to the terminal on its own"
          (> (count (term/output t)) (count before)) => true
          "the repaint clears the screen first so stale content from the old size is erased"
          (clojure.string/includes? (subs (term/output t) (count before)) "[2J") => true
          "the screen reflects the new dimensions"
          (count (app/screen-of app)) => 5
          (count (first (app/screen-of app))) => 20))))

  (component "a terminal below the root minimum paints a 'too small' message"
    (let [app (app/application {:root-class Root :initial-state true})
          t   (term/string-terminal {:rows 1 :cols 4})]
      ;; Root min defaults to 1x1, so make the message-trigger explicit by asking for a bigger min.
      (swap! (:com.fulcrologic.fulcro.application/state-atom app) assoc ::engine/focus :a)
      ;; Drive the too-small buffer directly (root min is 1x1 by default, so simulate via the helper).
      (let [buf (app/too-small-buffer 2 30 10 5)
            scr (engine/screen buf)]
        (assertions
          "the too-small buffer contains the required-dimensions message"
          (str/includes? (first scr) "too small") => true
          (str/includes? (first scr) "10x5") => true)))))

(specification {:covers {`app/step!         "6b509b,7a63d8"
                         `app/follow-focus! "f2b69d,09b8da"}} "viewport scrolling, follow-focus, and cursor tracking"
  (component "Tab into the list auto-scrolls the viewport so the focused item stays visible"
    (let [app (new-vp-app)
          t   (term/string-terminal {:rows 8 :cols 12})]
      (app/attach! app t)                                   ; focus item-0, list shows 0..3
      (assertions
        "initially focus is the first item and the list is unscrolled"
        (engine/current-focus app) => :item-0
        (engine/viewport-scroll app :list) => {:x 0 :y 0})
      (dotimes [_ 6] (app/step! app {:key :tab}))           ; focus reaches item-6
      (assertions
        "tabbing advances focus into the list"
        (engine/current-focus app) => :item-6
        "follow-focus advanced the viewport scroll so the focused item is visible"
        (engine/viewport-scroll app :list) => {:x 0 :y 3}
        "the focused item's text is on screen"
        (some #(str/includes? % "Item 6") (app/screen-of app)) => true
        "the hardware cursor tracks the focused item at its scrolled on-screen row (bottom of window)"
        (term/cursor t) => {:x 0 :y 3 :visible? true})))

  (component "arrowing Down through the focusable list moves focus and autoscrolls the viewport"
    (let [app (new-vp-app)
          t   (term/string-terminal {:rows 8 :cols 12})]
      (app/attach! app t)                                   ; focus item-0, list shows 0..3
      (assertions
        "initially focus is the first item and the list is unscrolled"
        (engine/current-focus app) => :item-0
        (engine/viewport-scroll app :list) => {:x 0 :y 0})
      (dotimes [_ 6] (app/step! app {:key :down}))          ; focus reaches item-6
      (assertions
        ":down moves focus item-to-item through the list"
        (engine/current-focus app) => :item-6
        "follow-focus autoscrolled the viewport so the focused item is visible"
        (engine/viewport-scroll app :list) => {:x 0 :y 3}
        "the focused item's text is on screen"
        (some #(str/includes? % "Item 6") (app/screen-of app)) => true)))

  (component "PageDown scrolls the focused viewport by a page"
    (let [app (new-vp-app)
          t   (term/string-terminal {:rows 8 :cols 12})]
      (app/attach! app t)                                   ; focus item-0
      (app/step! app {:key :page-down})
      (assertions
        "PageDown advances the viewport scroll by a page without moving focus out of the list"
        (engine/viewport-scroll app :list) => {:x 0 :y 3}
        "the deeper window is now on screen"
        (some #(str/includes? % "Item 3") (app/screen-of app)) => true
        "the focused item (item-0) is now scrolled out of view, so its cursor is hidden"
        (:visible? (term/cursor t)) => false)))

  (component "PageUp scrolls back toward the top"
    (let [app (new-vp-app)
          t   (term/string-terminal {:rows 8 :cols 12})]
      (app/attach! app t)
      (app/step! app {:key :page-down})                     ; scroll to y=3
      (app/step! app {:key :page-up})
      (assertions
        "PageUp scrolls the viewport back up by a page (clamped to 0)"
        (engine/viewport-scroll app :list) => {:x 0 :y 0}))))

(specification "multiline input — wrapping, Enter, arrow nav, internal scroll"
  (component "typing wraps the value across visual rows and tracks the cursor"
    (let [app (new-ml-app)
          t   (term/string-terminal {:rows 6 :cols 12})]
      (app/attach! app t)                                   ; focus :notes (the only focusable)
      (assertions
        "the multiline input is the initial focus"
        (engine/current-focus app) => :notes)
      (doseq [ch "the quick brown fox jumps"]
        (app/step! app {:key (str ch) :char (str ch)}))
      (assertions
        "the typed value is stored verbatim"
        (:ml/notes (state app)) => "the quick brown fox jumps"
        "the value is painted wrapped to the input width across successive rows"
        (take 3 (app/screen-of app)) => ["the quick   " "brown fox   " "jumps       "]
        "the hardware cursor sits at the end of the last wrapped row (row 2, col 5)"
        (term/cursor t) => {:x 5 :y 2 :visible? true})))

  (component "Enter inserts a newline and shows multiple rows"
    (let [app (new-ml-app)
          t   (term/string-terminal {:rows 6 :cols 12})]
      (app/attach! app t)
      (doseq [ch "ab"] (app/step! app {:key (str ch) :char (str ch)}))
      (app/step! app {:key :enter})
      (doseq [ch "cd"] (app/step! app {:key (str ch) :char (str ch)}))
      (assertions
        "Enter inserted a newline into the value (it did not submit)"
        (:ml/notes (state app)) => "ab\ncd"
        "the two hard lines paint on successive rows"
        (take 2 (app/screen-of app)) => ["ab          " "cd          "]
        "the cursor is at the end of the second line"
        (term/cursor t) => {:x 2 :y 1 :visible? true})))

  (component "Up/Down move the cursor between visual rows"
    (let [app (new-ml-app)
          t   (term/string-terminal {:rows 6 :cols 12})]
      (app/attach! app t)
      (doseq [ch "abc def ghi"] (app/step! app {:key (str ch) :char (str ch)}))
      ;; "abc def ghi" wraps at 9 to ["abc def"(0..6) "ghi"(8..10)]; caret at end is row 1, col 3.
      (assertions
        "the cursor starts on the last visual row at the caret column"
        (term/cursor t) => {:x 3 :y 1 :visible? true})
      (app/step! app {:key :up})
      (assertions
        ":up moves the cursor to the previous visual row keeping the column"
        (term/cursor t) => {:x 3 :y 0 :visible? true})
      (app/step! app {:key :down})
      (assertions
        ":down returns the cursor to the next visual row"
        (term/cursor t) => {:x 3 :y 1 :visible? true})))

  (component "internal scroll keeps the caret visible when content exceeds the height"
    (let [app (new-ml-app)                                  ; input height is 3 rows
          t   (term/string-terminal {:rows 8 :cols 12})]
      (app/attach! app t)
      ;; "the quick brown fox jumps over" wraps to 4 rows at width 9; caret at end is row 3.
      (doseq [ch "the quick brown fox jumps over"] (app/step! app {:key (str ch) :char (str ch)}))
      (assertions
        "the window scrolled so the caret's (last) wrapped row is the last visible row"
        (take 3 (app/screen-of app)) => ["brown fox   " "jumps       " "over        "]
        "the cursor is on the bottom visible row at the caret column"
        (term/cursor t) => {:x 4 :y 2 :visible? true})
      ;; move up to the top of the content; the window should scroll back to reveal row 0
      (dotimes [_ 3] (app/step! app {:key :up}))
      (assertions
        "scrolling back up reveals the first wrapped row again"
        (first (app/screen-of app)) => "the quick   "))))

(specification {:covers {`app/mount!        "b62698,7c992f"
                         `app/run-blocking! "ecfe25,b0b406"
                         `app/quit!         "336706,a354d9"}} "mount! / run! — input loop"
  (component "run! with a finite key script processes the keys and ends, leaving the terminal"
    (let [app (new-app)
          t   (term/string-terminal {:rows 10 :cols 30
                                     :keys [{:key "H" :char "H"}
                                            {:key "i" :char "i"}]})]
      ;; t-read-key dequeues the two keys then returns nil, so the loop terminates deterministically.
      (app/run-blocking! app {:terminal t})
      (assertions
        "the scripted keys were processed (state reflects the typed text)"
        (:app/a (state app)) => "Hi"
        "the loop terminated and left the terminal"
        (:left? @(.-state t)) => true)))

  (component "quit! stops a mounted loop and leaves the terminal"
    (let [app    (new-app)
          ;; a terminal that blocks (no keys, t-read-key returns nil immediately here so the
          ;; loop ends on its own); we still assert quit! flips running? and leaves.
          t      (term/string-terminal {:rows 10 :cols 30})
          handle (app/mount! app {:terminal t})]
      (Thread/sleep 50)
      (app/quit! handle)
      (assertions
        "quit! flips the running? flag false"
        (deref (:running? handle)) => false
        "quit! leaves the terminal"
        (:left? @(.-state t)) => true)))

  (component "quit! JOINS+CLEARS the render loop before leaving (so no frame paints onto the restored screen)"
    ;; Blocking terminal so the input loop is parked and the render loop is genuinely LIVE when quit! runs
    ;; (otherwise the input-loop finally would already have torn it down on EOF, making the join moot).
    (let [app    (new-app)
          {:keys [terminal release left?]} (blocking-terminal 10 30)
          handle (app/mount! app {:terminal terminal :max-fps 120})]
      (Thread/sleep 30)                                     ; let the dedicated render thread spin up
      (app/quit! handle)
      (deliver release nil)                                 ; unblock the parked read so the input thread can end
      (assertions
        ;; The bug: quit! only FLAGGED the render loop, then immediately left the terminal — so an
        ;; in-flight paint could land on the just-restored screen. shutdown! joins the thread first, so
        ;; by the time quit! returns the render thread is provably gone.
        "the dedicated render thread has exited by the time quit! returns (joined, not just flagged)"
        (.isAlive ^Thread (:render-thread handle)) => false
        ;; D: the runtime ::render-loop entry is removed, so a later request-render! does not enqueue onto
        ;; a dead thread (it falls back to a synchronous render, like a freshly-attached app).
        "the ::render-loop runtime entry is cleared"
        (render-loop-of app) => nil
        "quit! still flips running? false and leaves the terminal"
        [(deref (:running? handle)) @left?] => [false true])))

  (component "EOF exit tears down fully too (not only quit!): render thread stopped, ::render-loop cleared, terminal left"
    ;; Regression for the dirty-terminal fix's blind spot: run-blocking! reaching EOF used to leave the
    ;; terminal but leave the render thread ALIVE (it would then paint a closed terminal / leak a daemon).
    (let [app    (new-app)
          t      (term/string-terminal {:rows 10 :cols 30 :keys [{:key "a" :char "a"}]})
          handle (app/run-blocking! app {:terminal t :max-fps 120})]
      (assertions
        "the terminal was left"
        (:left? @(.-state t)) => true
        "the render thread was stopped — NOT leaked alive after EOF"
        (.isAlive ^Thread (:render-thread handle)) => false
        "the ::render-loop runtime entry was cleared"
        (render-loop-of app) => nil)))

  (component "quit! is idempotent — a second quit! is a safe no-op"
    (let [app    (new-app)
          {:keys [terminal release left?]} (blocking-terminal 10 30)
          handle (app/mount! app {:terminal terminal :max-fps 120})]
      (Thread/sleep 30)
      (app/quit! handle)
      (deliver release nil)                                 ; let the input thread reach EOF + its finally
      (app/quit! handle)                                    ; second call must not throw or re-break state
      (Thread/sleep 20)
      (assertions
        "the render thread is gone and stays gone"
        (.isAlive ^Thread (:render-thread handle)) => false
        "::render-loop stays cleared"
        (render-loop-of app) => nil
        "the terminal stays left"
        @left? => true)))

  (component "after shutdown a late request-render! cannot write to the left/closed terminal (clears ::terminal)"
    ;; shutdown! clears ::render-loop, so a later request-render! falls back to a SYNCHRONOUS render!.
    ;; Unless ::terminal is also cleared, that render writes to the now-left terminal (a real JLine throws
    ;; 'write after leave'). This fake throws on write once left, proving the post-shutdown no-op.
    (let [release (promise)
          left?   (atom false)
          t       (reify term/Terminal
                    (t-size [_] {:rows 5 :cols 10})
                    (t-read-key [_] @release)
                    (t-write! [_ _] (when @left? (throw (ex-info "write after leave" {}))))
                    (t-flush! [_] nil)
                    (t-set-cursor! [_ _ _ _] nil)
                    (t-enter! [_] nil)
                    (t-leave! [_] (reset! left? true) nil)
                    (t-sync-supported? [_] false)
                    (t-enhanced-keys? [_] false)
                    (t-on-resize! [_ _] nil))
          app     (new-app)
          handle  (app/mount! app {:terminal t :max-fps 120})]
      (app/quit! handle)                                    ; shutdown!: leaves terminal + clears ::terminal
      (deliver release nil)                                 ; release the parked input loop
      (assertions
        "quit! cleared ::terminal from the runtime"
        (app/terminal app) => nil
        "a late request-render! is a clean no-op — it does NOT write to the left terminal, and does not throw"
        (try (app/request-render! app) :ok (catch Throwable e [:threw (.getMessage e)])) => :ok)))

  (component "shutdown! deregisters a crash-restore ::shutdown-hook if present (cooperates with terminal-restore)"
    ;; The crash-restore feature stashes a JVM shutdown hook under ::shutdown-hook. The centralized
    ;; shutdown! drops it on EVERY teardown (keyword + interop, no dependency on that feature) so repeated
    ;; nonblocking mount!+quit! — even across fresh apps — never accumulate registered hooks.
    ;; Use a blocking terminal so the input loop PARKS (no immediate EOF): only quit!'s shutdown! runs
    ;; the teardown, so planting the hook then quit!ing is deterministic (no input-thread shutdown! racing
    ;; the plant on a string-terminal's immediate EOF).
    (let [app    (new-app)
          {:keys [terminal release]} (blocking-terminal 5 10)
          handle (app/mount! app {:terminal terminal :max-fps 120})
          hook   (Thread. ^Runnable (fn [] nil) "fake-restore-hook")]
      (.addShutdownHook (Runtime/getRuntime) hook)
      (swap! (:com.fulcrologic.fulcro.application/runtime-atom app)
        assoc :com.fulcrologic.fulcro.tui.application/shutdown-hook hook)
      (app/quit! handle)
      (deliver release nil)                                 ; let the parked input loop exit
      (assertions
        "quit!'s shutdown! cleared the ::shutdown-hook runtime entry"
        (:com.fulcrologic.fulcro.tui.application/shutdown-hook
          (deref (:com.fulcrologic.fulcro.application/runtime-atom app))) => nil
        "and deregistered the hook from the JVM (removeShutdownHook => false: already removed)"
        (.removeShutdownHook (Runtime/getRuntime) ^Thread hook) => false)))

  (component "a loop-level global keymap dispatches reserved chords (e.g. quit) regardless of focus"
    (let [app   (new-app)
          fired (atom false)
          t     (term/string-terminal {:rows 10 :cols 30
                                       :keys [{:key "q" :char "q" :ctrl? true}
                                              {:key "Z" :char "Z"}]})]
      (app/run-blocking! app {:terminal      t
                              :global-keymap {[:ctrl "q"] (fn [a _e] (reset! fired true) (app/quit! a))}})
      (assertions
        "the global chord handler fired for the ctrl-q chord"
        @fired => true
        "the loop stopped and left the terminal"
        (:left? @(.-state t)) => true
        "keys after the quit chord were not processed by the focus/input pipeline"
        (:app/a (state app)) => (:app/a (state (new-app)))))))

(specification {:covers {`app/run-render-loop! "971aca,4498f1"}}
  "live render loop — repaints a state change made off the input thread (decoupled rendering)"
  ;; A BLOCKING terminal parks the input loop in t-read-key, so the render loop stays alive because the
  ;; SESSION is live — we no longer rely on the render loop surviving an immediate input EOF (it must not).
  (let [app           (new-app)
        {:keys [terminal release left?]} (blocking-terminal 10 30)
        handle        (app/mount! app {:terminal terminal :max-fps 120})]
    ;; A state change NOT driven by a keystroke (stands in for a statechart/async update). It only flags
    ;; dirty on THIS thread; the render loop must pick it up and repaint.
    (comp/transact! app [(set-a {:v "ZZ"})])
    (let [painted? (loop [n 0]
                     (cond
                       ;; screen-of reads the LAST PAINTED buffer (it does not render) — so a match
                       ;; proves the render loop, not this thread, did the paint.
                       (str/starts-with? (or (nth (app/screen-of app) 0 nil) "") "ZZ") true
                       (>= n 200) false
                       :else (do (Thread/sleep 10) (recur (inc n)))))]
      ;; End the session by EOF-ing the read; the input-loop finally then runs the FULL teardown.
      (deliver release nil)
      (Thread/sleep 60)
      (assertions
        "the dedicated render loop repaints an off-input-thread state change without an explicit render!"
        painted? => true
        "EOF then tears down fully — the terminal is left AND the render thread is stopped, not leaked"
        [@left? (.isAlive ^Thread (:render-thread handle))] => [true false]))))

;; ---------------------------------------------------------------------------
;; Overlay / picker: a root with a launch button and a state-gated picker.
;; ---------------------------------------------------------------------------

(m/defmutation open-picker [_] (action [{:keys [state]}] (swap! state assoc :pk/open? true)))
(m/defmutation close-picker [_] (action [{:keys [state]}] (swap! state assoc :pk/open? false)))
(m/defmutation pick [{:keys [v]}] (action [{:keys [state]}] (swap! state assoc :pk/open? false :pk/choice v)))

(comp/defsc PickerRoot [this {:keys [pk/open? pk/choice]}]
  {:query         [:pk/open? :pk/choice]
   :ident         (fn [] [:component/id ::pkroot])
   :initial-state {:pk/open? false :pk/choice nil}}
  (elements/vbox {:id "root"}
    (elements/button {:id :launch :on-activate (fn [] (comp/transact! this [(open-picker {})]))}
      (str "Choice: " (or choice "none")))
    (elements/button {:id :other} "Other")
    (elements/picker {:id        :fruit
                      :open?     open?
                      :title     "Fruit"
                      :width     16
                      :height    6
                      :options   (mapv (fn [i] {:value (keyword (str "f" i)) :label (str "Fruit " i)}) (range 10))
                      :on-select (fn [v] (comp/transact! this [(pick {:v v})]))
                      :on-cancel (fn [] (comp/transact! this [(close-picker {})]))})))

(defn- new-picker-app []
  (app/application {:root-class PickerRoot :initial-state true}))

(specification {:covers {`app/render! "b81026,8cfecc"
                         `app/step!   "6b509b,7a63d8"}} "overlay compositing & focus trap"
  (component "while the picker is closed only the base UI is focusable and painted"
    (let [app (new-picker-app)
          t   (term/string-terminal {:rows 10 :cols 24})]
      (app/attach! app t)
      (assertions
        "initial focus is a base control, not anything inside the closed picker"
        (engine/current-focus app) => :launch
        "Tab cycles only among base controls (the closed picker's rows are not in the ring)"
        (do (app/step! app {:key :tab}) (engine/current-focus app)) => :other
        (do (app/step! app {:key :tab}) (engine/current-focus app)) => :launch
        "the closed picker paints nothing (the base launch row is on row 0)"
        (str/starts-with? (nth (app/screen-of app) 0) "Choice: none") => true)))

  (component "opening the picker composites it on top and traps focus inside it"
    (let [app (new-picker-app)
          t   (term/string-terminal {:rows 10 :cols 24})]
      (app/attach! app t)                                   ; focus :launch
      (app/step! app {:key :enter})                         ; activate launch -> open picker
      (let [scr (app/screen-of app)]
        (assertions
          "the picker's modal state is open"
          (:pk/open? (state app)) => true
          "focus moves into the picker (its first row)"
          (engine/current-focus app) => :fruit-f0
          "the modal window is painted on top with its title border"
          (str/includes? (nth scr 2) "Fruit") => true
          "Tab cycles only among the picker's rows — base controls are inert"
          (do (app/step! app {:key :tab}) (engine/current-focus app)) => :fruit-f1)
        (assertions
          "Tab never escapes to a base control while the picker is open"
          (do (dotimes [_ 20] (app/step! app {:key :tab}))
              (contains? (set (map (fn [i] (keyword (str "f" i))) (range 10)))
                (keyword (name (engine/current-focus app))))
              (str/starts-with? (name (engine/current-focus app)) "fruit-f")) => true))))

  (component "Escape dismisses the picker and returns focus to the base UI"
    (let [app (new-picker-app)
          t   (term/string-terminal {:rows 10 :cols 24})]
      (app/attach! app t)
      (app/step! app {:key :enter})                         ; open
      (app/step! app {:key :escape})                        ; dismiss
      (let [scr (app/screen-of app)]
        (assertions
          "the picker's :on-cancel ran (modal state closed)"
          (:pk/open? (state app)) => false
          "focus returns to a base control"
          (engine/current-focus app) => :launch
          "the base UI paints un-obscured again (no modal border on screen)"
          (str/includes? (apply str scr) "Fruit") => false)))))

(specification {:covers {`app/step!         "6b509b,7a63d8"
                         `app/follow-focus! "f2b69d,09b8da"}} "picker scrolling and selection"
  (component "arrowing down past the visible rows auto-scrolls the picker's viewport"
    (let [app (new-picker-app)
          t   (term/string-terminal {:rows 10 :cols 24})]
      (app/attach! app t)
      (app/step! app {:key :enter})                         ; open; rows 0..3 visible
      (assertions
        "the list starts unscrolled with the first row focused"
        (engine/current-focus app) => :fruit-f0
        (engine/viewport-scroll app :fruit-list) => {:x 0 :y 0})
      (dotimes [_ 5] (app/step! app {:key :down}))          ; focus reaches f5
      (assertions
        "focus advances to the fifth row"
        (engine/current-focus app) => :fruit-f5
        "follow-focus scrolled the picker's viewport to keep the focused row visible"
        (pos? (:y (engine/viewport-scroll app :fruit-list))) => true)))

  (component "Enter on a focused row selects that option and closes the picker"
    (let [app (new-picker-app)
          t   (term/string-terminal {:rows 10 :cols 24})]
      (app/attach! app t)
      (app/step! app {:key :enter})                         ; open
      (app/step! app {:key :down})                          ; focus f1
      (app/step! app {:key :enter})                         ; select f1
      (assertions
        "the focused row's value is recorded via :on-select"
        (:pk/choice (state app)) => :f1
        "selecting closes the picker"
        (:pk/open? (state app)) => false
        "focus returns to the base launch control"
        (engine/current-focus app) => :launch
        "the base UI reflects the chosen value"
        (str/starts-with? (nth (app/screen-of app) 0) "Choice: :f1") => true))))

;; ---------------------------------------------------------------------------
;; Entrypoint niceties: start!, app-level global-keymap, lifecycle (C1/C2).
;; ---------------------------------------------------------------------------

(specification {:covers {`app/start! "f91686,6adee3"}} "start! — build + run in one call"
  (component "start! builds the app (application) and runs it to completion (run-blocking!)"
    (let [t      (term/string-terminal {:rows 10 :cols 30
                                        :keys [{:key "H" :char "H"}
                                               {:key "i" :char "i"}]})
          handle (app/start! {:root-class Root :initial-state true} {:terminal t})]
      (assertions
        "the built app processed the scripted keys (state reflects the typed text)"
        (:app/a (state (:app handle))) => "Hi"
        "the loop ran to completion and left the terminal"
        (:left? @(.-state t)) => true))))

(specification "entrypoint lifecycle — resize-unregister (C1), loop catch (C2), app-level global-keymap"
  (component "quit! unregisters the resize handler, so a later resize does not repaint the terminal (C1)"
    (let [app (new-app)
          t   (term/string-terminal {:rows 10 :cols 30})]
      (app/attach! app t)
      (let [before (count (term/output t))]
        (term/resize! t 12 40)
        (let [after-active (count (term/output t))]
          (app/quit! app)
          (let [post-quit (count (term/output t))]
            (term/resize! t 8 20)
            (assertions
              "while attached, a resize repaints (terminal output grows)"
              (> after-active before) => true
              "after quit!, a resize produces no further output (handler was cleared)"
              (count (term/output t)) => post-quit))))))

  (component "redraw! forces a full clear+repaint to recover a corrupted screen"
    (let [app (new-app)
          t   (term/string-terminal {:rows 8 :cols 24})]
      (app/attach! app t)                                   ; initial render
      (let [before (count (term/output t))]
        (app/redraw! app)
        (let [new-out (subs (term/output t) before)]
          (assertions
            "redraw! emits another frame (a repaint occurred)"
            (pos? (count new-out)) => true
            "that frame cleared the screen (full repaint, not a diff)"
            (str/includes? new-out "2J") => true
            "the force-redraw flag is consumed after the frame"
            (get @(:com.fulcrologic.fulcro.application/runtime-atom app)
              :com.fulcrologic.fulcro.tui.application/force-redraw?) => false)))))

  (component "a forced redraw whose write THROWS re-arms ::force-redraw? for the next frame (consumed edge is not lost)"
    ;; render! consumes ::force-redraw? (swap-vals!) before the terminal write. If the write then throws,
    ;; the next frame would diff against a baseline the screen never received unless the flag is re-armed.
    (let [app   (new-app)
          boom? (atom false)
          t     (reify term/Terminal
                  (t-size [_] {:rows 6 :cols 20})
                  (t-read-key [_] nil)
                  (t-write! [_ _] (when @boom? (throw (ex-info "write boom" {}))))
                  (t-flush! [_] nil)
                  (t-set-cursor! [_ _ _ _] nil)
                  (t-enter! [_] nil)
                  (t-leave! [_] nil)
                  (t-sync-supported? [_] false)
                  (t-enhanced-keys? [_] false)
                  (t-on-resize! [_ _] nil))]
      (app/attach! app t)                                   ; initial paint OK (boom? still false)
      (reset! boom? true)
      ;; Arm the flag directly (redraw! would synchronously render+consume it on the no-loop test path).
      (swap! (:com.fulcrologic.fulcro.application/runtime-atom app)
        assoc :com.fulcrologic.fulcro.tui.application/force-redraw? true)
      (assertions
        "render! propagates the terminal write failure"
        (try (app/render! app) ::no-throw (catch Throwable _ ::threw)) => ::threw
        "the consumed force-redraw? flag was re-armed for the next frame"
        (get @(:com.fulcrologic.fulcro.application/runtime-atom app)
          :com.fulcrologic.fulcro.tui.application/force-redraw?) => true)))

  (component "an exception while handling a keystroke is TOLERATED: recorded, passed to :on-error, loop keeps running (C2)"
    (let [app    (new-app)
          calls  (atom 0)
          boom   (ex-info "boom" {})
          ;; Two throwing keystrokes: if the loop survives the first it will handle the second too.
          t      (term/string-terminal {:rows 10 :cols 30
                                        :keys [{:key "q" :char "q" :ctrl? true}
                                               {:key "q" :char "q" :ctrl? true}]})
          handle (app/run-blocking! app {:terminal      t
                                         :global-keymap {[:ctrl "q"] (fn [_a _e] (throw boom))}
                                         :on-error      (fn [_app _th] (swap! calls inc))})]
      (assertions
        ":on-error fired for BOTH throwing keystrokes — the loop survived the first and kept running"
        @calls => 2
        "the most recent error is recorded for sane reporting (last-error), rather than being fatal"
        (app/last-error app) => boom
        "the handle's :error atom stays nil — a handler exception no longer kills the loop"
        (deref (:error handle)) => nil
        "the terminal is still left when the loop ends (EOF)"
        (:left? @(.-state t)) => true)))

  (component "a :global-keymap registered at application time is used when run-blocking! is given none"
    (let [fired (atom false)
          app   (app/application {:root-class    Root :initial-state true
                                  :global-keymap {[:ctrl "q"] (fn [a _e] (reset! fired true) (app/quit! a))}})
          t     (term/string-terminal {:rows 10 :cols 30
                                       :keys [{:key "q" :char "q" :ctrl? true}
                                              {:key "Z" :char "Z"}]})]
      (app/run-blocking! app {:terminal t})
      (assertions
        "the app-level keymap fires the reserved chord"
        @fired => true
        "the loop stopped and left the terminal"
        (:left? @(.-state t)) => true))))

(def ^:private dirty-key :com.fulcrologic.fulcro.tui.application/dirty?)
(def ^:private render-loop-key :com.fulcrologic.fulcro.tui.application/render-loop)

(specification {:covers {`app/mark-dirty! "9d0878,089ec1"}} "mark-dirty!"
  (let [runtime-key :com.fulcrologic.fulcro.application/runtime-atom
        app         (new-app)
        dirty       (get @(get app runtime-key) dirty-key)]
    (assertions
      "the app is built with a (clear) dirty flag"
      @dirty => false)
    (app/mark-dirty! app)
    (assertions
      "sets the app's dirty flag so the render loop will repaint"
      @dirty => true)))

(specification {:covers {`app/request-render! "f8787f,877481"}}
  "request-render! — synchronous when no render loop, else defers to it"
  (let [runtime-key :com.fulcrologic.fulcro.application/runtime-atom]
    (component "no render loop installed renders synchronously, one render per request"
      (let [app   (new-app)
            t     (term/string-terminal {:rows 10 :cols 30})
            _     (app/attach! app t)                        ; one initial render
            calls (atom 0)]
        (with-redefs [app/render! (fn [a] (swap! calls inc) a)]
          (dotimes [_ 5] (app/request-render! app)))
        (assertions
          "every request renders synchronously (no render loop present)"
          @calls => 5)))

    (component "a LIVE render loop present defers painting to it (flags dirty, never paints on the caller)"
      (let [app   (new-app)
            t     (term/string-terminal {:rows 10 :cols 30})
            _     (app/attach! app t)
            dirty (get @(get app runtime-key) dirty-key)
            ;; A stand-in render loop that is genuinely ALIVE and running: request-render! must route to
            ;; mark-dirty! rather than paint on the caller.
            th    (doto (Thread. ^Runnable (fn [] (try (Thread/sleep 5000) (catch Throwable _ nil))))
                    (.setDaemon true) (.start))
            calls (atom 0)]
        (swap! (get app runtime-key) assoc render-loop-key {:thread th :running? (atom true)})
        (reset! dirty false)
        (with-redefs [app/render! (fn [a] (swap! calls inc) a)]
          (app/request-render! app))
        (.interrupt th)
        (assertions
          "does NOT render on the calling thread"
          @calls => 0
          "flags the app dirty so the render loop repaints"
          @dirty => true)))

    (component "a STOPPED/dead render-loop entry renders synchronously (does not enqueue onto a dead loop)"
      (let [app   (new-app)
            t     (term/string-terminal {:rows 10 :cols 30})
            _     (app/attach! app t)
            ;; The entry lingers with :running? true, but the thread is NOT alive (never started) — models
            ;; a loop that has exited while its runtime entry has not yet been cleared.
            th    (Thread. ^Runnable (fn []))
            calls (atom 0)]
        (swap! (get app runtime-key) assoc render-loop-key {:thread th :running? (atom true)})
        (with-redefs [app/render! (fn [a] (swap! calls inc) a)]
          (app/request-render! app))
        (assertions
          "renders synchronously because the loop thread is not alive"
          @calls => 1)))))
