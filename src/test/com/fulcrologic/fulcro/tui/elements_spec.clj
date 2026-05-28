(ns com.fulcrologic.fulcro.tui.elements-spec
  (:require
    [com.fulcrologic.fulcro.tui.elements :as elements]
    [com.fulcrologic.fulcro.tui.engine :as engine]
    [fulcro-spec.core :refer [=> assertions component specification]]))

(specification {:covers {`elements/element "abe4b1,2fa298"}} "element generators"
  (component "attribute handling"
    (assertions
      "uses a leading map as the node's attributes"
      (::engine/attrs (elements/vbox {:width 3} "x")) => {:width 3}
      "defaults attributes to an empty map when the first arg is not a map"
      (::engine/attrs (elements/vbox "x")) => {}))

  (component "children"
    (assertions
      "keeps node and string children in order"
      (::engine/children (elements/vbox {} "a" (elements/text {} "b"))) => ["a" (elements/text {} "b")]
      "flattens nested sequences spliced into the children"
      (::engine/children (elements/vbox {} (list "a" "b") "c")) => ["a" "b" "c"]
      "removes nil children"
      (::engine/children (elements/vbox {} "a" nil "b")) => ["a" "b"]))

  (component "tags"
    (assertions
      "each generator stamps its own tag"
      (::engine/tag (elements/vbox)) => :vbox
      (::engine/tag (elements/hbox)) => :hbox
      (::engine/tag (elements/box)) => :box
      (::engine/tag (elements/text)) => :text
      (::engine/tag (elements/button)) => :button
      (::engine/tag (elements/line)) => :line
      (::engine/tag (elements/viewport)) => :viewport
      (::engine/tag (elements/modal {})) => :modal))

  (component "input"
    (assertions
      "is a childless leaf carrying only its attributes"
      (elements/input {:id :x :value "hi"})
      => {::engine/tag :input ::engine/attrs {:id :x :value "hi"} ::engine/children []}))

  (component "modal"
    (assertions
      "passes through its :id and :open? attributes"
      (engine/node-attr (elements/modal {:id :dlg :open? true}) :id) => :dlg
      (engine/node-attr (elements/modal {:id :dlg :open? true}) :open?) => true
      "defaults :border? to true"
      (engine/node-attr (elements/modal {:id :d}) :border?) => true
      "lets a caller override the border default"
      (engine/node-attr (elements/modal {:id :d :border? false}) :border?) => false
      "keeps its children (stacked like a vbox)"
      (mapv ::engine/tag (::engine/children (elements/modal {:id :d} (elements/text {} "hi")))) => [:text])))

(specification {:covers {`elements/focused? "95dd3d,8a7d8e"}} "focused?"
  (assertions
    "is true for the bound *current-focus*"
    (binding [engine/*current-focus* "name"] (elements/focused? "name")) => true
    "is false for a different id"
    (binding [engine/*current-focus* "name"] (elements/focused? "other")) => false))

(specification {:covers {`elements/picker "f322af,2c7344"}} "picker"
  (let [selected  (atom nil)
        cancelled (atom false)
        p         (elements/picker {:id        :fruit :open? true :title "Fruit" :width 16 :height 6
                                    :options   [{:value :apple :label "Apple"}
                                                {:value :pear :label "Pear"}]
                                    :on-select (fn [v] (reset! selected v))
                                    :on-cancel (fn [] (reset! cancelled true))})
        viewport  (first (::engine/children p))
        list-box  (first (::engine/children viewport))
        rows      (::engine/children list-box)]
    (assertions
      "is a modal carrying the picker's id, title, and size"
      (::engine/tag p) => :modal
      (engine/node-attr p :id) => :fruit
      (engine/node-attr p :title) => "Fruit"
      "wraps its rows in a scrollable viewport keyed off the picker id"
      (::engine/tag viewport) => :viewport
      (engine/node-attr viewport :id) => :fruit-list
      "renders one focusable button row per option, in order"
      (mapv ::engine/tag rows) => [:button :button]
      (mapv #(engine/node-attr % :id) rows) => [:fruit-apple :fruit-pear]
      (mapv #(apply str (::engine/children %)) rows) => ["Apple" "Pear"]
      "wires the modal's :on-dismiss to the picker's :on-cancel"
      (do ((engine/node-attr p :on-dismiss)) @cancelled) => true
      "activating a row selects that option's value via :on-select"
      (do (engine/activate! (first rows)) @selected) => :apple)))
