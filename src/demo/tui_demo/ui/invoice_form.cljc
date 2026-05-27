(ns tui-demo.ui.invoice-form
  "RAD (statechart) form for editing/creating an Invoice, with a to-many LineItem
   subform and a pick-one customer reference. The invoice total is derived from the
   line-item subtotals."
  (:require
    [com.fulcrologic.fulcro.algorithms.tempid :as tempid]
    [com.fulcrologic.fulcro.raw.components :as rc]
    [com.fulcrologic.rad.form-options :as fo]
    [com.fulcrologic.rad.picker-options :as po]
    [com.fulcrologic.rad.statechart.form :refer [defsc-form]]
    [com.fulcrologic.rad.type-support.decimal :as math]
    [tui-demo.model.invoice :as invoice]
    [tui-demo.ui.line-item-form :refer [LineItemForm]]))

(defsc-form InvoiceForm [this props]
  {fo/id            invoice/id
   fo/title         (fn [_ {:invoice/keys [id]}]
                      (if (tempid/tempid? id) "New Invoice" "Edit Invoice"))
   fo/attributes    [invoice/customer invoice/date invoice/line-items invoice/total]
   fo/field-styles  {:invoice/customer :pick-one}
   fo/field-options {:invoice/customer
                     {po/query-key       :account/all-accounts
                      po/query-component (rc/nc [:account/id :account/name] {:ident :account/id})
                      po/options-xform   (fn [_ accounts]
                                           (mapv (fn [{:account/keys [id name]}]
                                                   {:text name :value [:account/id id]})
                                             accounts))}}
   fo/subforms      {:invoice/line-items {fo/ui          LineItemForm
                                          fo/can-add?    (fn [_ _] true)
                                          fo/can-delete? (fn [_ _] true)}}
   fo/layout        [[:invoice/customer :invoice/date]
                     [:invoice/line-items]
                     [:invoice/total]]
   fo/triggers      {:derive-fields
                     (fn [{:invoice/keys [line-items] :as invoice}]
                       (assoc invoice
                         :invoice/total (reduce (fn [acc {:line-item/keys [subtotal]}]
                                                  (math/+ acc (or subtotal (math/zero))))
                                          (math/zero) line-items)))}})
