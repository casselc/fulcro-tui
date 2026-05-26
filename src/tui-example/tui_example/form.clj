(ns tui-example.form
  "A small runnable demo of the Fulcro TUI rendering target: a two-field form with a Save button,
   plus a scrolling viewport of selectable items below it.

   Run it in a real terminal with:

     clojure -M:tui -m tui-example.form

   Controls: Tab / Shift-Tab move focus (through the fields, the multiline notes box, the Save button,
   then into the item list), type to edit the focused field, Enter or Space activates the focused Save
   button or list item, PageUp / PageDown scroll the item list, and Ctrl-Q quits. Tabbing into the
   list auto-scrolls the viewport to keep the focused item visible.

   The notes box is a multiline text-area: it wraps to its width, Enter inserts a newline (it does not
   submit), the arrow keys move the caret (Up/Down by visual line), and it self-scrolls to keep the
   caret visible. A read-only wrapped paragraph above the form demonstrates line wrapping."
  (:require
    [com.fulcrologic.fulcro.mutations :as m]
    [com.fulcrologic.fulcro.tui :as tui]
    [com.fulcrologic.fulcro.tui.driver :as drv]))

(m/defmutation set-field
  "Stores value `v` under top-level key `k` in app state (the form fields live at the root)."
  [{:keys [k v]}]
  (action [{:keys [state]}] (swap! state assoc k v)))

(def item-count
  "How many selectable items the scrolling list contains."
  15)

(def initial-db
  "The initial root-level app state for the demo (the form fields live at the db root)."
  {:form/name "" :form/email "" :form/notes "" :form/saved? false :form/selected nil})

(def lorem
  "A read-only sentence used to demonstrate word wrapping in a fixed-width box."
  "This read-only paragraph wraps to its fixed-width box, showing the word-wrapping support.")

(tui/defsc Root [this {:keys [form/name form/email form/notes form/saved? form/selected]}]
  ;; A flat root: its data lives at the top of the app db (no :ident/:initial-state, which
  ;; keeps this example free of `com.fulcrologic.fulcro.components` and thus babashka-clean).
  {:query [:form/name :form/email :form/notes :form/saved? :form/selected]}
  (let [field (fn [label id value k]
                (tui/hbox {:height 1}
                  (tui/text {:width 8} label)
                  (tui/input {:id        id
                              :grow      1
                              :value     (or value "")
                              :on-change (fn [v _caret]
                                           ;; editing a field clears the prior "Saved!" status
                                           (tui/transact! this [(set-field {:k k :v v})
                                                                (set-field {:k :form/saved? :v false})]))})))]
    (tui/vbox {:padding 1 :border? true}
      (tui/text {:bold true} "Fulcro TUI demo")
      (tui/text {} "Tab/Shift-Tab move focus · type to edit · Enter/Space activates · Ctrl-Q quits")
      (tui/text {} "PageUp/PageDown scroll the item list below")
      (tui/line {})
      ;; A read-only paragraph that wraps to its fixed-width bordered box (40 wide => 38 content
      ;; columns; the box is tall enough to show all 5 wrapped lines).
      (tui/text {} "About (read-only, wrapped):")
      (tui/box {:width 40 :height 5 :border? true}
        (tui/text {:wrap true} lorem))
      (tui/line {})
      (field "Name:"  :name  name  :form/name)
      (field "Email:" :email email :form/email)
      (tui/line {})
      ;; An editable multiline text-area wired to :form/notes. It grows to the available width,
      ;; is 4 rows tall, wraps its value, and self-scrolls to keep the caret visible.
      (tui/text {} "Notes (multiline · Enter=newline · arrows move caret):")
      ;; Fixed height (4 rows) and fills the available width on the cross-axis. NOT :grow —
      ;; a fixed-size text box should not fight for leftover vertical space.
      (tui/input {:id         :notes
                  :multiline? true
                  :height     4
                  :value      (or notes "")
                  :on-change  (fn [v _caret]
                                (tui/transact! this [(set-field {:k :form/notes :v v})
                                                     (set-field {:k :form/saved? :v false})]))})
      (tui/line {})
      (tui/button {:id          :save
                   :highlight   (tui/focused? :save)
                   :on-activate (fn [] (tui/transact! this [(set-field {:k :form/saved? :v true})]))}
        (if saved? " Saved! " " Save "))
      (tui/text {} (str "name=" (pr-str name) "  email=" (pr-str email) "  saved?=" (boolean saved?)))
      (tui/line {})
      (tui/text {} (str "Items (Tab in, then arrows/PageUp/PageDown to scroll) — "
                     "selected: " (if selected (str "Item " selected) "none")))
      ;; A fixed-height viewport of 15 focusable items; tabbing into it auto-scrolls.
      (tui/viewport {:id :items :height 6 :border? true}
        (tui/vbox {}
          (for [i (range item-count)]
            (tui/button {:id          (keyword (str "item-" i))
                         :highlight   (tui/focused? (keyword (str "item-" i)))
                         :on-activate (fn [] (tui/transact! this [(set-field {:k :form/selected :v i})]))}
              (str "Item " i))))))))

(defn -main
  "Builds the demo app and runs it on the system terminal until Ctrl-Q."
  [& _args]
  (let [app (drv/application {:root-class Root})]
    (reset! (:com.fulcrologic.fulcro.application/state-atom app) initial-db)
    (drv/run-blocking! app {:global-keymap {[:ctrl "q"] (fn [a _e] (drv/quit! a))}})
    (println "Goodbye.")))
