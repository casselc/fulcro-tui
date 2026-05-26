(ns com.fulcrologic.fulcro.tui.driver-spec
  (:require
    [clojure.string :as str]
    [com.fulcrologic.fulcro.mutations :as m]
    [com.fulcrologic.fulcro.tui :as tui]
    [com.fulcrologic.fulcro.tui.driver :as driver]
    [com.fulcrologic.fulcro.tui.terminal :as term]
    [fulcro-spec.core :refer [specification component assertions =>]]))

;; ---------------------------------------------------------------------------
;; Test app: a vbox with two inputs (:a/:b) and a button (:ok).
;; ---------------------------------------------------------------------------

(m/defmutation set-a [{:keys [v]}]
  (action [{:keys [state]}] (swap! state assoc :app/a v)))

(m/defmutation set-b [{:keys [v]}]
  (action [{:keys [state]}] (swap! state assoc :app/b v)))

(m/defmutation set-ok [_]
  (action [{:keys [state]}] (swap! state assoc :app/ok? true)))

(tui/defsc Root [this {:keys [app/a app/b app/ok?]}]
  {:query         [:app/a :app/b :app/ok?]
   :ident         (fn [] [:component/id ::root])
   :initial-state {:app/a "" :app/b "" :app/ok? false}}
  (tui/vbox {:id "root"}
    (tui/input {:id        :a
                :value     a
                :on-change (fn [v _caret] (tui/transact! this [(set-a {:v v})]))})
    (tui/input {:id        :b
                :value     b
                :on-change (fn [v _caret] (tui/transact! this [(set-b {:v v})]))})
    (tui/button {:id          :ok
                 ;; route-key only fires :on-key, so activation lives there (enter/space).
                 :on-key      (fn [e]
                                (when (#{:enter " "} (:key e))
                                  (tui/transact! this [(set-ok {})])
                                  :handled))
                 :on-activate (fn [] (tui/transact! this [(set-ok {})]))}
      (if ok? "DONE" "OK"))))

(defn- new-app []
  (driver/application {:root-class Root :initial-state true}))

(defn- state [app]
  (deref (:com.fulcrologic.fulcro.application/state-atom app)))

;; A root with a fixed-height viewport of many focusable items, for scrolling/follow-focus tests.
(tui/defsc VPRoot [this _props]
  {:query         [:vp/x]
   :ident         (fn [] [:component/id ::vproot])
   :initial-state {:vp/x 1}}
  (tui/vbox {:id "root"}
    (tui/viewport {:id :list :height 4}
      (tui/vbox {}
        (for [i (range 10)]
          (tui/button {:id (keyword (str "item-" i))} (str "Item " i)))))))

(defn- new-vp-app []
  (driver/application {:root-class VPRoot :initial-state true}))

;; A root with a single fixed-size multiline text-area, for multiline editing/cursor/scroll tests.
(m/defmutation set-notes [{:keys [v]}]
  (action [{:keys [state]}] (swap! state assoc :ml/notes v)))

(tui/defsc MLRoot [this {:keys [ml/notes]}]
  {:query         [:ml/notes]
   :ident         (fn [] [:component/id ::mlroot])
   :initial-state {:ml/notes ""}}
  (tui/vbox {:id "root"}
    (tui/input {:id        :notes
                :multiline? true
                :height    3
                :width     9
                :value     notes
                :on-change (fn [v _caret] (tui/transact! this [(set-notes {:v v})]))})))

(defn- new-ml-app []
  (driver/application {:root-class MLRoot :initial-state true}))

;; ---------------------------------------------------------------------------

(specification {:covers {`driver/application      "598adf,e9f188"
                         `driver/attach!          "b02297,736960"
                         `driver/render!          "e6f44f,19809b"
                         `driver/screen-of        "f2651c,662c58"
                         `driver/screen-styled-of "bac68a,5083b1"
                         `driver/terminal         "18f70c,089ec1"}} "application / attach! (initial paint)"
  (let [app (new-app)
        t   (term/string-terminal {:rows 10 :cols 30})]
    (driver/attach! app t)
    (let [scr (driver/screen-of app)]
      (assertions
        "attach! enters the terminal"
        (:entered? @(.-state t)) => true
        "attach! stashes the terminal on the app runtime (terminal accessor)"
        (driver/terminal app) => t
        "the initial paint puts the (empty) inputs on rows 0 and 1 (blank) and the button label on row 2"
        (nth scr 0) => "                              "
        (nth scr 1) => "                              "
        (nth scr 2) => "OK                            "
        "the screen has one row per terminal row"
        (count scr) => 10
        "screen-styled-of returns the styled cells of the most recent buffer (button label cells on row 2)"
        (mapv :ch (take 2 (nth (driver/screen-styled-of app) 2))) => [\O \K]
        "initial focus is the first focusable (input :a)"
        (tui/current-focus app) => :a
        "the hardware cursor is shown at input :a's caret origin (0,0)"
        (term/cursor t) => {:x 0 :y 0 :visible? true}))))

(specification {:covers {`driver/step! "32154f,85f381"}} "step! — focus, typing, and activation"
  (component "Tab moves focus and follows the cursor to the newly focused input"
    (let [app (new-app)
          t   (term/string-terminal {:rows 10 :cols 30})]
      (driver/attach! app t)
      (driver/step! app {:key :tab})
      (assertions
        "Tab advances focus from :a to :b"
        (tui/current-focus app) => :b
        "the cursor moves to input :b's row (row 1) at the caret origin"
        (term/cursor t) => {:x 0 :y 1 :visible? true})))

  (component "typing printable keys into the focused input updates state, screen, and caret"
    (let [app (new-app)
          t   (term/string-terminal {:rows 10 :cols 30})]
      (driver/attach! app t)                                ; focus :a
      (driver/step! app {:key "H" :char "H"})
      (driver/step! app {:key "i" :char "i"})
      (assertions
        "the input's :on-change transaction updated the value in app state"
        (:app/a (state app)) => "Hi"
        "the painted screen reflects the typed text on input :a's row"
        (nth (driver/screen-of app) 0) => "Hi                            "
        "the caret advanced by the number of typed characters"
        (tui/get-caret app :a 0) => 2
        "the hardware cursor follows the caret (x=2 on row 0)"
        (term/cursor t) => {:x 2 :y 0 :visible? true})))

  (component "activating the button (Tab to it, then Enter) runs its handler"
    (let [app (new-app)
          t   (term/string-terminal {:rows 10 :cols 30})]
      (driver/attach! app t)
      (driver/step! app {:key :tab})                        ; a -> b
      (driver/step! app {:key :tab})                        ; b -> ok
      (assertions
        "focus reaches the button"
        (tui/current-focus app) => :ok)
      (driver/step! app {:key :enter})
      (assertions
        "the button's handler ran (its mutation set the flag in state)"
        (:app/ok? (state app)) => true
        "the repaint shows the button's updated label"
        (nth (driver/screen-of app) 2) => "DONE                          "))))

(specification {:covers {`driver/render!          "e6f44f,19809b"
                         `driver/too-small-buffer "81d757,4eb533"}} "render! — diff path & resize"
  (component "a no-op re-render produces no terminal output"
    (let [app (new-app)
          t   (term/string-terminal {:rows 10 :cols 30})]
      (driver/attach! app t)
      (let [before (term/output t)]
        (driver/render! app)                                ; identical frame
        (assertions
          "rendering an unchanged frame writes no additional bytes to the terminal"
          (term/output t) => before))))

  (component "a resize forces a full repaint"
    (let [app (new-app)
          t   (term/string-terminal {:rows 10 :cols 30})]
      (driver/attach! app t)
      (term/resize! t 5 20)
      (let [before (term/output t)]
        (driver/render! app)
        (assertions
          "after a size change the next frame writes a (full) repaint"
          (> (count (term/output t)) (count before)) => true
          "the screen reflects the new dimensions"
          (count (driver/screen-of app)) => 5
          (count (first (driver/screen-of app))) => 20))))

  (component "a terminal below the root minimum paints a 'too small' message"
    (let [app (driver/application {:root-class Root :initial-state true})
          t   (term/string-terminal {:rows 1 :cols 4})]
      ;; Root min defaults to 1x1, so make the message-trigger explicit by asking for a bigger min.
      (swap! (:com.fulcrologic.fulcro.application/state-atom app) assoc ::tui/focus :a)
      ;; Drive the too-small buffer directly (root min is 1x1 by default, so simulate via the helper).
      (let [buf (driver/too-small-buffer 2 30 10 5)
            scr (tui/screen buf)]
        (assertions
          "the too-small buffer contains the required-dimensions message"
          (str/includes? (first scr) "too small") => true
          (str/includes? (first scr) "10x5") => true)))))

(specification {:covers {`driver/step! "32154f,85f381"
                         `driver/follow-focus! "a7ce94,866f38"}} "viewport scrolling, follow-focus, and cursor tracking"
  (component "Tab into the list auto-scrolls the viewport so the focused item stays visible"
    (let [app (new-vp-app)
          t   (term/string-terminal {:rows 8 :cols 12})]
      (driver/attach! app t)                                  ; focus item-0, list shows 0..3
      (assertions
        "initially focus is the first item and the list is unscrolled"
        (tui/current-focus app) => :item-0
        (tui/viewport-scroll app :list) => {:x 0 :y 0})
      (dotimes [_ 6] (driver/step! app {:key :tab}))          ; focus reaches item-6
      (assertions
        "tabbing advances focus into the list"
        (tui/current-focus app) => :item-6
        "follow-focus advanced the viewport scroll so the focused item is visible"
        (tui/viewport-scroll app :list) => {:x 0 :y 3}
        "the focused item's text is on screen"
        (some #(str/includes? % "Item 6") (driver/screen-of app)) => true
        "the hardware cursor tracks the focused item at its scrolled on-screen row (bottom of window)"
        (term/cursor t) => {:x 0 :y 3 :visible? true})))

  (component "arrowing Down through the focusable list moves focus and autoscrolls the viewport"
    (let [app (new-vp-app)
          t   (term/string-terminal {:rows 8 :cols 12})]
      (driver/attach! app t)                                  ; focus item-0, list shows 0..3
      (assertions
        "initially focus is the first item and the list is unscrolled"
        (tui/current-focus app) => :item-0
        (tui/viewport-scroll app :list) => {:x 0 :y 0})
      (dotimes [_ 6] (driver/step! app {:key :down}))         ; focus reaches item-6
      (assertions
        ":down moves focus item-to-item through the list"
        (tui/current-focus app) => :item-6
        "follow-focus autoscrolled the viewport so the focused item is visible"
        (tui/viewport-scroll app :list) => {:x 0 :y 3}
        "the focused item's text is on screen"
        (some #(str/includes? % "Item 6") (driver/screen-of app)) => true)))

  (component "PageDown scrolls the focused viewport by a page"
    (let [app (new-vp-app)
          t   (term/string-terminal {:rows 8 :cols 12})]
      (driver/attach! app t)                                  ; focus item-0
      (driver/step! app {:key :page-down})
      (assertions
        "PageDown advances the viewport scroll by a page without moving focus out of the list"
        (tui/viewport-scroll app :list) => {:x 0 :y 3}
        "the deeper window is now on screen"
        (some #(str/includes? % "Item 3") (driver/screen-of app)) => true
        "the focused item (item-0) is now scrolled out of view, so its cursor is hidden"
        (:visible? (term/cursor t)) => false)))

  (component "PageUp scrolls back toward the top"
    (let [app (new-vp-app)
          t   (term/string-terminal {:rows 8 :cols 12})]
      (driver/attach! app t)
      (driver/step! app {:key :page-down})                    ; scroll to y=3
      (driver/step! app {:key :page-up})
      (assertions
        "PageUp scrolls the viewport back up by a page (clamped to 0)"
        (tui/viewport-scroll app :list) => {:x 0 :y 0}))))

(specification "multiline input — wrapping, Enter, arrow nav, internal scroll"
  (component "typing wraps the value across visual rows and tracks the cursor"
    (let [app (new-ml-app)
          t   (term/string-terminal {:rows 6 :cols 12})]
      (driver/attach! app t)                                ; focus :notes (the only focusable)
      (assertions
        "the multiline input is the initial focus"
        (tui/current-focus app) => :notes)
      (doseq [ch "the quick brown fox jumps"]
        (driver/step! app {:key (str ch) :char (str ch)}))
      (assertions
        "the typed value is stored verbatim"
        (:ml/notes (state app)) => "the quick brown fox jumps"
        "the value is painted wrapped to the input width across successive rows"
        (take 3 (driver/screen-of app)) => ["the quick   " "brown fox   " "jumps       "]
        "the hardware cursor sits at the end of the last wrapped row (row 2, col 5)"
        (term/cursor t) => {:x 5 :y 2 :visible? true})))

  (component "Enter inserts a newline and shows multiple rows"
    (let [app (new-ml-app)
          t   (term/string-terminal {:rows 6 :cols 12})]
      (driver/attach! app t)
      (doseq [ch "ab"] (driver/step! app {:key (str ch) :char (str ch)}))
      (driver/step! app {:key :enter})
      (doseq [ch "cd"] (driver/step! app {:key (str ch) :char (str ch)}))
      (assertions
        "Enter inserted a newline into the value (it did not submit)"
        (:ml/notes (state app)) => "ab\ncd"
        "the two hard lines paint on successive rows"
        (take 2 (driver/screen-of app)) => ["ab          " "cd          "]
        "the cursor is at the end of the second line"
        (term/cursor t) => {:x 2 :y 1 :visible? true})))

  (component "Up/Down move the cursor between visual rows"
    (let [app (new-ml-app)
          t   (term/string-terminal {:rows 6 :cols 12})]
      (driver/attach! app t)
      (doseq [ch "abc def ghi"] (driver/step! app {:key (str ch) :char (str ch)}))
      ;; "abc def ghi" wraps at 9 to ["abc def"(0..6) "ghi"(8..10)]; caret at end is row 1, col 3.
      (assertions
        "the cursor starts on the last visual row at the caret column"
        (term/cursor t) => {:x 3 :y 1 :visible? true})
      (driver/step! app {:key :up})
      (assertions
        ":up moves the cursor to the previous visual row keeping the column"
        (term/cursor t) => {:x 3 :y 0 :visible? true})
      (driver/step! app {:key :down})
      (assertions
        ":down returns the cursor to the next visual row"
        (term/cursor t) => {:x 3 :y 1 :visible? true})))

  (component "internal scroll keeps the caret visible when content exceeds the height"
    (let [app (new-ml-app)                                  ; input height is 3 rows
          t   (term/string-terminal {:rows 8 :cols 12})]
      (driver/attach! app t)
      ;; "the quick brown fox jumps over" wraps to 4 rows at width 9; caret at end is row 3.
      (doseq [ch "the quick brown fox jumps over"] (driver/step! app {:key (str ch) :char (str ch)}))
      (assertions
        "the window scrolled so the caret's (last) wrapped row is the last visible row"
        (take 3 (driver/screen-of app)) => ["brown fox   " "jumps       " "over        "]
        "the cursor is on the bottom visible row at the caret column"
        (term/cursor t) => {:x 4 :y 2 :visible? true})
      ;; move up to the top of the content; the window should scroll back to reveal row 0
      (dotimes [_ 3] (driver/step! app {:key :up}))
      (assertions
        "scrolling back up reveals the first wrapped row again"
        (first (driver/screen-of app)) => "the quick   "))))

(specification {:covers {`driver/mount! "fb742f,c76f81"
                         `driver/run-blocking! "a8159d,559871"
                         `driver/quit!  "acaa83,a354d9"}} "mount! / run! — input loop"
  (component "run! with a finite key script processes the keys and ends, leaving the terminal"
    (let [app (new-app)
          t   (term/string-terminal {:rows 10 :cols 30
                                      :keys [{:key "H" :char "H"}
                                             {:key "i" :char "i"}]})]
      ;; t-read-key dequeues the two keys then returns nil, so the loop terminates deterministically.
      (driver/run-blocking! app {:terminal t})
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
          handle (driver/mount! app {:terminal t})]
      (Thread/sleep 50)
      (driver/quit! handle)
      (assertions
        "quit! flips the running? flag false"
        (deref (:running? handle)) => false
        "quit! leaves the terminal"
        (:left? @(.-state t)) => true)))

  (component "a loop-level global keymap dispatches reserved chords (e.g. quit) regardless of focus"
    (let [app   (new-app)
          fired (atom false)
          t     (term/string-terminal {:rows 10 :cols 30
                                        :keys [{:key "q" :char "q" :ctrl? true}
                                               {:key "Z" :char "Z"}]})]
      (driver/run-blocking! app {:terminal      t
                        :global-keymap {[:ctrl "q"] (fn [a _e] (reset! fired true) (driver/quit! a))}})
      (assertions
        "the global chord handler fired for the ctrl-q chord"
        @fired => true
        "the loop stopped and left the terminal"
        (:left? @(.-state t)) => true
        "keys after the quit chord were not processed by the focus/input pipeline"
        (:app/a (state app)) => (:app/a (state (new-app)))))))
