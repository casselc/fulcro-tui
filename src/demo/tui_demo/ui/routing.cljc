(ns tui-demo.ui.routing
  "The application-level statechart (the \"statechart-as-app\" pattern): a single chart
   owns routing for the whole demo. Reports and forms are declared as route states; the
   report routes to the form via `form/edit!`/`create!`."
  (:require
    [com.fulcrologic.rad.statechart.form :as form]
    [com.fulcrologic.rad.statechart.report :as report]
    [com.fulcrologic.statecharts.chart :refer [statechart]]
    [com.fulcrologic.statecharts.integration.fulcro :as scf]
    [com.fulcrologic.statecharts.integration.fulcro.routing :as scr]
    [tui-demo.ui.account-forms :refer [AccountForm AccountList]]
    [tui-demo.ui.invoice-form :refer [InvoiceForm]]
    [tui-demo.ui.invoice-report :refer [InvoiceReport]]
    [tui-demo.ui.item-forms :refer [InventoryReport ItemForm]]
    [tui-demo.ui.root :refer [Routes]]))

(def routing-chart
  "One chart for the whole app: a routes region containing every report and its edit form.
   Reports route to their forms via `form/edit!`/`create!`; the root nav switches between reports."
  (statechart {:initial :state/route-root}
    (scr/routing-regions
      (scr/routes {:id :state/root :routing/root Routes}
        (report/report-route-state {:route/target InvoiceReport})
        (form/form-route-state {:route/target InvoiceForm :route/params #{:id}})
        (report/report-route-state {:route/target AccountList})
        (form/form-route-state {:route/target AccountForm :route/params #{:id}})
        (report/report-route-state {:route/target InventoryReport})
        (form/form-route-state {:route/target ItemForm :route/params #{:id}})))))

(defn install!
  "Installs the statechart engine on `app` and starts the routing chart.

   Uses the asynchronous event loop (`:event-loop? true`, a core.async go-loop that runs
   under babashka too). This matters: a report load completes via a network callback that
   then sends `:event/loaded`, and the loop drains that follow-on event so the report
   advances load → process → ready. `:immediate` only runs the directly-dispatched event,
   leaving the report stuck busy with no processed rows."
  [app]
  (scf/install-fulcro-statecharts! app {:event-loop? true})
  (scr/start! app routing-chart))
