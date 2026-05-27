(ns tui-demo.ui.line-item-form
  "RAD (statechart) subform for a single invoice line item. Edited only as a child
   of `tui-demo.ui.invoice-form/InvoiceForm`."
  (:require
    [com.fulcrologic.rad.form-options :as fo]
    [com.fulcrologic.rad.statechart.form :refer [defsc-form]]
    [com.fulcrologic.rad.type-support.decimal :as math]
    [tui-demo.model.line-item :as line-item]))

(defsc-form LineItemForm [this props]
  {fo/id         line-item/id
   fo/attributes [line-item/description line-item/quantity line-item/unit-price line-item/subtotal]
   fo/layout     [[:line-item/description]
                  [:line-item/quantity :line-item/unit-price :line-item/subtotal]]
   fo/triggers   {:derive-fields
                  (fn [{:line-item/keys [quantity unit-price] :as line-item}]
                    (assoc line-item
                      :line-item/subtotal (math/* (or quantity 0) (or unit-price (math/zero)))))}})
