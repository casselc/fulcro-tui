(ns tui-demo.client.main
  "Entry point for the terminal client. Builds the synchronous fulcro-tui app, installs
   the TUI RAD rendering plugin + the statechart routing, points a transit HTTP remote at
   the demo server (localhost:3001), routes to the invoice report, and runs the input loop."
  (:require
    [com.fulcrologic.fulcro.data-fetch :as df]
    [com.fulcrologic.fulcro.raw.components :as rc]
    [com.fulcrologic.fulcro.tui.application :as tui-app]
    [com.fulcrologic.rad.application :as rad-app]
    [com.fulcrologic.rad.type-support.date-time :as dt]
    [com.fulcrologic.statecharts.integration.fulcro.routing :as scr]
    [taoensso.timbre :as log]
    [tui-demo.client.remote :as remote]
    [tui-demo.rendering.tui.plugin :as tui-plugin]
    [tui-demo.ui.invoice-report :refer [InvoiceReport]]
    [tui-demo.ui.root :as root]
    [tui-demo.ui.routing :as routing]))

(def AccountOption
  "A normalizing component for loading the account picker options into the `:account/id` table."
  (rc/nc [:account/id :account/name]
    {:ident         (fn [_ props] [:account/id (:account/id props)])
     :componentName ::AccountOption}))

(defn new-app
  "Builds and wires the demo client app (no terminal attached yet). `base-url` is the
   server origin, e.g. \"http://localhost:3001\"."
  [base-url]
  (let [app (tui-app/application
              {:root-class    root/Root
               :remotes       {:remote (remote/transit-remote base-url)}
               :global-keymap {[:ctrl "q"] (fn [a _] (tui-app/quit! a))}})]
    (rad-app/install-ui-controls! app tui-plugin/all-controls)
    (routing/install! app)
    ;; Preload the account list so the invoice form's customer pick-one has options to cycle.
    ;; (A render-time po/load-options! does not reliably populate the cache in the synchronous TUI.)
    (df/load! app :account/all-accounts AccountOption)
    app))

(defn -main [& _args]
  ;; Statecharts log at DEBUG very verbosely; keep the terminal clean.
  (log/merge-config! {:min-level :warn})
  ;; RAD's instant formatters resolve against a bound timezone; without one they NPE (blank dates).
  (dt/set-timezone! "America/Los_Angeles")
  (let [app (new-app "http://localhost:3001")]
    (scr/route-to! app InvoiceReport)
    (tui-app/run-blocking! app)))
