(ns tui-demo.client.main
  "Entry point for the terminal client. Builds the synchronous fulcro-tui app, installs
   the TUI RAD rendering plugin + the statechart routing, points a transit HTTP remote at
   the demo server (localhost:3001), routes to the invoice report, and runs the input loop."
  (:require
    [com.fulcrologic.fulcro.data-fetch :as df]
    [com.fulcrologic.fulcro.raw.components :as rc]
    [com.fulcrologic.fulcro.tui.application :as tui-app]
    [com.fulcrologic.rad.application :as rad-app]
    [com.fulcrologic.rad.control :as control]
    [com.fulcrologic.rad.picker-options :as po]
    [com.fulcrologic.rad.type-support.date-time :as dt]
    [com.fulcrologic.statecharts.integration.fulcro.routing :as scr]
    [taoensso.timbre :as log]
    [tui-demo.client.remote :as remote]
    [tui-demo.rendering.tui.plugin :as tui-plugin]
    [tui-demo.ui.item-forms :refer [InventoryReport]]
    [tui-demo.ui.invoice-report :refer [InvoiceReport]]
    [tui-demo.ui.root :as root]
    [tui-demo.ui.routing :as routing]))

(def AccountOption
  "A normalizing component for loading the account picker options into the `:account/id` table."
  (rc/nc [:account/id :account/name]
    {:componentName ::AccountOption}))

(defn new-app
  "Builds and wires the demo client app (no terminal attached yet). `base-url` is the
   server origin, e.g. \"http://localhost:3001\"."
  [base-url]
  (let [app (tui-app/application
              {:root-class    root/Root
               :remotes       {:remote (remote/transit-remote base-url)}
               :global-keymap {[:ctrl "q"] (fn [a _] (tui-app/quit! a))
                               ;; Force a full clean repaint — recovers the screen if stray output
                               ;; (e.g. a log line) corrupts it.
                               [:ctrl "l"] (fn [a _] (tui-app/redraw! a))}})]
    (rad-app/install-ui-controls! app tui-plugin/all-controls)
    (routing/install! app)
    ;; Preload the account list so the invoice form's customer pick-one has options immediately.
    (df/load! app :account/all-accounts AccountOption)
    ;; Preload the Inventory report's category-filter picker options (the picker control renders pure —
    ;; it never self-loads — so we seed its options cache here using the report's own control config).
    (let [ctl (get-in (rc/component-options InventoryReport)
                [::control/controls :tui-demo.ui.item-forms/category])]
      (po/load-picker-options! app InventoryReport {} ctl))
    app))

(defn -main [& _args]
  ;; Logs to stdout would corrupt the TUI frame; send them to a temp file instead. Statecharts also
  ;; log very verbosely at DEBUG, so keep the file readable at :info.
  (let [logfile (tui-app/redirect-logging-to-temp-file!)]
    (log/merge-config! {:min-level :info})
    (when logfile (binding [*out* *err*] (println "Logging to" (.getAbsolutePath logfile))))
    ;; RAD's instant formatters resolve against a bound timezone; without one they NPE (blank dates).
    (dt/set-timezone! "America/Los_Angeles")
    (let [app (new-app "http://localhost:3001")]
      (scr/route-to! app InvoiceReport)
      (tui-app/run-blocking! app))
    ;; The input loop has ended (Ctrl-Q / EOF). Background threads (remote HTTP client, core.async
    ;; dispatch, Inspect socket) are non-daemon and would keep the JVM alive, so exit explicitly.
    (shutdown-agents)
    (System/exit 0)))
