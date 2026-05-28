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
    [com.fulcrologic.rad.statechart.form-options :as sfo]
    [com.fulcrologic.rad.type-support.decimal :as math]
    [com.fulcrologic.statecharts.integration.fulcro.routing :as scr]
    [tui-demo.model.invoice :as invoice]
    [tui-demo.model.line-item :as line-item]
    [tui-demo.ui.line-item-form :refer [LineItemForm]]))

(def report-route
  "Registry keyword of the invoice report (used as a route target without a require cycle)."
  :tui-demo.ui.invoice-report/InvoiceReport)

(defsc-form InvoiceForm [this props]
  {fo/id            invoice/id
   fo/title         (fn [_ {:invoice/keys [id]}]
                      (if (tempid/tempid? id) "New Invoice" "Edit Invoice"))
   ;; On cancel/save, route back to the report. (Default cancel-route is :back, which uses URL
   ;; history — the TUI has no history provider, so we name the target explicitly. A registry
   ;; keyword avoids a require cycle with the report ns.)
   fo/cancel-route  report-route
   fo/attributes    [invoice/customer invoice/date invoice/line-items invoice/total]
   fo/field-styles  {:invoice/customer :pick-one}
   fo/field-options {:invoice/customer
                     {po/query-key       :account/all-accounts
                      po/query-component (rc/nc [:account/id :account/name] {:componentName ::CustomerQuery})
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
   sfo/triggers     {;; Subform charts don't run — only the master's does — so the per-form picker-option
                     ;; loader never fires for the LineItem subform. Load its (category-independent)
                     ;; category picker options here when the invoice form starts; the item picker is
                     ;; category-dependent and loads via the LineItemForm `:on-change` cascade.
                     :started
                     (fn [{:fulcro/keys [app]} _data _form-ident]
                       (po/load-options! app LineItemForm {} line-item/category)
                       nil)
                     ;; On EDIT, each existing line item already has a category, but its item picker's
                     ;; options are category-scoped and only load on a category CHANGE (the `:on-change`
                     ;; cascade). Without this the saved item can't be matched to an option, so the field
                     ;; shows "(choose)". After the invoice + line items load, load each line item's
                     ;; category-scoped item options so the current item displays.
                     :after-load
                     (fn [{:fulcro/keys [app]} {:fulcro/keys [state-map]} form-ident]
                       (doseq [li-ident (:invoice/line-items (get-in state-map form-ident))]
                         (po/load-options! app LineItemForm (get-in state-map li-ident) line-item/item))
                       nil)
                     :derive-fields
                     (fn [{:invoice/keys [line-items] :as invoice}]
                       (assoc invoice
                         :invoice/total (reduce (fn [acc {:line-item/keys [subtotal]}]
                                                  (math/+ acc (or subtotal (math/zero))))
                                          (math/zero) line-items)))
                     ;; After a successful save, return to the report (on-saved doesn't navigate).
                     :saved
                     (fn [{:fulcro/keys [app]} _data _form-ident]
                       (scr/route-to! app report-route {})
                       nil)}})
