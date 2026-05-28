(ns tui-demo.ui.invoice-report
  "RAD (statechart) report listing all invoices. Rows route to the InvoiceForm for
   editing; a control creates a new invoice."
  (:require
    [com.fulcrologic.rad.report-options :as ro]
    [com.fulcrologic.rad.statechart.form :as form]
    [com.fulcrologic.rad.statechart.report :refer [defsc-report]]
    [com.fulcrologic.rad.type-support.decimal :as math]
    [tui-demo.model.invoice :as invoice]
    [tui-demo.ui.invoice-form :refer [InvoiceForm]]))

(defsc-report InvoiceReport [this props]
  {ro/title             "Invoices"
   ro/source-attribute  :invoice/all-invoices
   ro/row-pk            invoice/id
   ro/columns           [invoice/customer invoice/date invoice/total]
   ro/column-formatters {:invoice/total (fn [_ v _ _] (str "$" (math/numeric->str (or v (math/zero)))))}
   ro/form-links        {:invoice/total InvoiceForm}
   ro/row-actions       [{:label  "Edit"
                          :action (fn [report-instance {:invoice/keys [id]}]
                                    (form/edit! report-instance InvoiceForm id))}]
   ro/controls          {::new {:type     :button
                                :local?   true
                                :label    "New Invoice"
                                :shortcut [:alt "c"]
                                :action   (fn [report-instance] (form/create! report-instance InvoiceForm))}}
   ro/control-layout    {:action-buttons [::new]}
   ro/paginate?         true
   ro/page-size         40
   ro/run-on-mount?     true
   ro/route             "invoices"})
