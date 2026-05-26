(ns tui-example.form
  "A small runnable demo of the Fulcro TUI rendering target: a two-field form with a Save button,
   plus a scrolling viewport of selectable items below it.

   Run it in a real terminal with:

     clojure -M:tui -m tui-example.form

   Controls: Tab / Shift-Tab move focus (through the fields, the multiline notes box, the Save button,
   then into the item list), type to edit the focused field, Enter or Space activates the focused Save
   button or list item, PageUp / PageDown scroll the item list, and Ctrl-Q quits. Tabbing into the
   list auto-scrolls the viewport to keep the focused item visible.

   The \"Reset…\" button opens a plain `tui/modal` confirmation dialog with two focus-trapped buttons:
   Tab / Shift-Tab cycle between Reset and Cancel, Enter activates the focused one, and Escape (or
   Cancel) dismisses it. The \"Pick a fruit\" button opens a `tui/picker` — a modal list picker — that
   likewise overlays the UI and traps focus, so Up / Down move the highlight (the list scrolls to
   follow), Enter chooses the fruit, and Escape cancels. Both modals' open/closed state and any chosen
   value are ordinary app state.

   The notes box is a multiline text-area: it wraps to its width, Enter inserts a newline (it does not
   submit), the arrow keys move the caret (Up/Down by visual line), and it self-scrolls to keep the
   caret visible. A read-only wrapped paragraph above the form demonstrates line wrapping."
  (:require
   [clojure.string :as str]
   [com.fulcrologic.fulcro.mutations :as m]
   [com.fulcrologic.fulcro.tui :as tui]
   [com.fulcrologic.fulcro.tui.driver :as drv]))

(m/defmutation set-field
  "Stores value `v` under top-level key `k` in app state (the form fields live at the root)."
  [{:keys [k v]}]
  (action [{:keys [state]}] (swap! state assoc k v)))

(declare initial-db)

(m/defmutation reset-form
  "Clears every form field back to its initial value (used by the confirm-reset modal)."
  [_]
  (action [{:keys [state]}] (swap! state merge initial-db)))

(def item-count
  "How many selectable items the scrolling list contains."
  15)

(def initial-db
  "The initial root-level app state for the demo (the form fields live at the db root)."
  {:form/name        "" :form/email "" :form/notes "" :form/saved? false :form/selected nil
   :form/fruit       nil
   :form/picker-open?  false
   :form/confirm-open? false})

(def fruit-options
  "Options for the demo fruit picker — a list long enough to require scrolling."
  (mapv (fn [f] {:value (keyword f) :label (str/capitalize f)})
        ["apple" "banana" "cherry" "date" "elderberry" "fig" "grape"
         "honeydew" "kiwi" "lemon" "mango" "nectarine" "orange" "pear"]))

(def lorem
  "A read-only sentence used to demonstrate word wrapping in a fixed-width box."
  "This read-only paragraph wraps to its fixed-width box, showing the word-wrapping support.")

(tui/defsc Root [this {:keys [form/name form/email form/notes form/saved? form/selected
                              form/fruit form/picker-open? form/confirm-open?]}]
  ;; A flat root: its data lives at the top of the app db (no :ident/:initial-state, which
  ;; keeps this example free of `com.fulcrologic.fulcro.components` and thus babashka-clean).
  {:query [:form/name :form/email :form/notes :form/saved? :form/selected
           :form/fruit :form/picker-open? :form/confirm-open?]}
  (let [field (fn [label id value k]
                (tui/hbox {:height 1}
                          (tui/text {:width 8 :color :cyan} label)
                          (tui/input {:id        id
                                      :grow      1
                                      :color     :bright-white
                                      :value     (or value "")
                                      :on-change (fn [v _caret]
                                           ;; editing a field clears the prior "Saved!" status
                                                   (tui/transact! this [(set-field {:k k :v v})
                                                                        (set-field {:k :form/saved? :v false})]))})))]
    (tui/vbox {:padding 1 :border? true :color :cyan}
              (tui/text {:bold true :color :bright-cyan} "Fulcro TUI demo")
              (tui/text {:color :bright-black} "Tab/Shift-Tab move focus · type to edit · Enter/Space activates · Ctrl-Q quits")
              (tui/text {:color :bright-black} "PageUp/PageDown scroll the item list below")
              (tui/line {})
      ;; A read-only paragraph that wraps to its fixed-width bordered box (40 wide => 38 content
      ;; columns; the box is tall enough to show all 5 wrapped lines).
              (tui/text {:color :yellow} "About (read-only, wrapped):")
              (tui/box {:width 40 :height 5 :border? true :color :bright-black}
                       (tui/text {:wrap true} lorem))
              (tui/line {})
              (field "Name:"  :name  name  :form/name)
              (field "Email:" :email email :form/email)
              (tui/line {})
      ;; An editable multiline text-area wired to :form/notes. It grows to the available width,
      ;; is 4 rows tall, wraps its value, and self-scrolls to keep the caret visible.
              (tui/text {:color :cyan} "Notes (multiline · Enter=newline · arrows move caret):")
      ;; Fixed height (4 rows) and fills the available width on the cross-axis. NOT :grow —
      ;; a fixed-size text box should not fight for leftover vertical space.
              (tui/input {:id         :notes
                          :multiline? true
                          :height     4
                          :color      :bright-white
                          :value      (or notes "")
                          :on-change  (fn [v _caret]
                                        (tui/transact! this [(set-field {:k :form/notes :v v})
                                                             (set-field {:k :form/saved? :v false})]))})
              (tui/line {})
              (tui/hbox {:height 1}
                        (tui/button {:id          :save
                                     :color       (if saved? :bright-green :green)
                                     :bold        true
                                     :highlight   (tui/focused? :save)
                                     :on-activate (fn [] (tui/transact! this [(set-field {:k :form/saved? :v true})]))}
                                    (if saved? " Saved! " " Save "))
                        (tui/text {:width 2} "")
                ;; Opens a plain modal confirmation dialog (see the :confirm modal below).
                        (tui/button {:id          :reset
                                     :color       :bright-yellow
                                     :bold        true
                                     :highlight   (tui/focused? :reset)
                                     :on-activate (fn [] (tui/transact! this [(set-field {:k :form/confirm-open? :v true})]))}
                                    " Reset… "))
              (tui/text {:color (if saved? :bright-green :bright-black)}
                        (str "name=" (pr-str name) "  email=" (pr-str email) "  saved?=" (boolean saved?)))
              (tui/line {})
              (tui/text {:color :yellow} (str "Items (Tab in, then arrows/PageUp/PageDown to scroll) — "
                                              "selected: " (if selected (str "Item " selected) "none")))
      ;; A fixed-height viewport of 15 focusable items; tabbing into it auto-scrolls.
              (tui/viewport {:id :items :height 6 :border? true :color :bright-black}
                            (tui/vbox {}
                                      (for [i (range item-count)]
                                        (tui/button {:id          (keyword (str "item-" i))
                                                     :color       (if (= selected i) :bright-green :cyan)
                                                     :highlight   (tui/focused? (keyword (str "item-" i)))
                                                     :on-activate (fn [] (tui/transact! this [(set-field {:k :form/selected :v i})]))}
                                                    (str (if (= selected i) "● " "  ") "Item " i)))))
              (tui/line {})
      ;; A button that opens a modal list picker. The picker overlays the whole UI, traps focus
      ;; (Up/Down to highlight, Enter to choose, Escape to cancel), and scrolls if the list is long.
              (tui/button {:id          :pick-fruit
                           :color       :bright-magenta
                           :bold        true
                           :highlight   (tui/focused? :pick-fruit)
                           :on-activate (fn [] (tui/transact! this [(set-field {:k :form/picker-open? :v true})]))}
                          (str " Pick a fruit (" (if fruit (str/capitalize (clojure.core/name fruit)) "none") ") "))
      ;; The picker lives in the render tree always; it is shown only while :open? is truthy. Its
      ;; presence/selection are ordinary app state driven by the mutations above (the library owns none).
              (tui/picker {:id        :fruit
                           :open?     picker-open?
                           :title     "Pick a fruit"
                           :width     24
                           :height    8
                           :options   fruit-options
                           :on-select (fn [v] (tui/transact! this [(set-field {:k :form/fruit :v v})
                                                                   (set-field {:k :form/picker-open? :v false})]))
                           :on-cancel (fn [] (tui/transact! this [(set-field {:k :form/picker-open? :v false})]))})
      ;; A plain `tui/modal` used directly (not via `picker`): a confirmation dialog with its own
      ;; focus-trapped buttons. While open, Tab/Shift-Tab cycle only between Reset and Cancel, Enter
      ;; activates the focused one, and Escape (or Cancel) dismisses it.
              (let [close! (fn [] (tui/transact! this [(set-field {:k :form/confirm-open? :v false})]))]
                (tui/modal {:id         :confirm
                            :open?      confirm-open?
                            :title      "Confirm reset"
                            :width      40
                            :height     7
                            :color      :red
                            :on-dismiss close!}
                           (tui/text {:wrap true :color :bright-red}
                                     "Reset every field to its initial value? This cannot be undone.")
                           (tui/line {})
                           (tui/hbox {:height 1 :align :center}
                                     (tui/button {:id          :confirm-reset
                                                  :color       :bright-red
                                                  :bold        true
                                                  :highlight   (tui/focused? :confirm-reset)
                                                  :on-activate (fn [] (tui/transact! this [(reset-form {})])
                                                                 (close!))}
                                                 " Reset ")
                                     (tui/text {:width 2} "")
                                     (tui/button {:id          :confirm-cancel
                                                  :color       :bright-green
                                                  :bold        true
                                                  :highlight   (tui/focused? :confirm-cancel)
                                                  :on-activate close!}
                                                 " Cancel ")))))))

(defn -main
  "Builds the demo app and runs it on the system terminal until Ctrl-Q."
  [& _args]
  (let [app (drv/application {:root-class Root})]
    (reset! (:com.fulcrologic.fulcro.application/state-atom app) initial-db)
    (drv/run-blocking! app {:global-keymap {[:ctrl "q"] (fn [a _e] (drv/quit! a))}})
    (println "Goodbye.")))
