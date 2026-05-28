(ns tui-demo.rendering.tui-render-spec
  "Full-stack headless rendering tests for the TUI RAD plugin: a real client app (statechart
   routing + RAD reports/forms + the fulcro-tui rendering plugin) wired to the demo's Pathom
   parser through an in-process loopback remote (no HTTP), backed by a freshly-seeded in-memory
   Datomic Local database. Drives the app with a `string-terminal` and asserts on the painted
   screen — exercising report formatters, sortable headers, enum/boolean fields, and ref pickers
   exactly as a user would see them. JVM-only (Datomic Local does not run under babashka).

   Loads/routes are asynchronous (the statechart event loop + remote), so the helpers POLL the
   rendered screen until the expected content appears rather than sleeping a fixed time — important
   because the suite runs with throwing guardrails (`:all` mode), which slows everything down."
  (:require
    [clojure.string :as str]
    [com.fulcrologic.fulcro.headless.loopback-remotes :as lb]
    [com.fulcrologic.fulcro.raw.components :as rc]
    [com.fulcrologic.fulcro.tui.application :as tui-app]
    [com.fulcrologic.fulcro.tui.engine :as engine]
    [com.fulcrologic.fulcro.tui.terminal :as term]
    [com.fulcrologic.rad.application :as rad-app]
    [com.fulcrologic.rad.picker-options :as po]
    [com.fulcrologic.rad.type-support.date-time :as dt]
    [com.fulcrologic.statecharts.integration.fulcro.routing :as scr]
    [fulcro-spec.core :refer [=> assertions specification]]
    [taoensso.timbre :as log]
    [tui-demo.rendering.tui.plugin :as tui-plugin]
    [tui-demo.server.datomic-spec :as dspec]
    [tui-demo.server.parser :as parser]
    [tui-demo.ui.root :as root]
    [tui-demo.ui.routing :as routing]))

(defn- screen-text
  "Returns the painted screen of `app` as a single newline-joined string (trailing blanks trimmed)."
  [app]
  (tui-app/render! app)
  (str/join "\n" (mapv str/trimr (tui-app/screen-of app))))

(defn- wait-for
  "Renders `app` and returns the screen text once it matches `re`, polling up to ~20s. Returns the
   last screen on timeout (so the assertion fails with the real screen, not a hang)."
  [app re]
  (loop [n 0]
    (let [screen (screen-text app)]
      (if (or (re-find re screen) (>= n 80))
        screen
        (do (Thread/sleep 250) (recur (inc n)))))))

(defn- test-app
  "Builds a headless demo client app whose remote calls the Pathom parser in-process, attaches a
   `string-terminal`, and returns it. Assumes a seeded connection is already installed on the parser."
  []
  (let [app (tui-app/application
              {:root-class root/Root
               :remotes    {:remote (lb/sync-remote (fn [eql] (parser/process-eql eql)))}})]
    (rad-app/install-ui-controls! app tui-plugin/all-controls)
    (routing/install! app)
    (tui-app/attach! app (term/string-terminal {:rows 30 :cols 100}))
    app))

(defn- fresh-app!
  "Seeds a fresh in-memory DB, sets the timezone, and returns a routed headless app."
  []
  (dspec/fresh-test-conn!)
  (dt/set-timezone! "America/Los_Angeles")
  (test-app))

(specification "report rendering (Accounts)"
  (log/with-merged-config {:min-level :error}
    (let [app    (fresh-app!)
          _      (scr/route-to! app :tui-demo.ui.account-forms/AccountList {})
          ;; Wait until the seeded rows have loaded (Alice present).
          screen (wait-for app #"Alice")]
      (assertions
        "renders sortable Name + Active? column headers"
        (boolean (re-find #"Name.*Active\?" screen)) => true
        "marks the active sort column with an arrow"
        (boolean (re-find #"Name [▲▼]" screen)) => true
        "renders the boolean Active? column via the Yes/No formatter (not raw true/false)"
        (and (boolean (re-find #"\bYes\b" screen)) (not (boolean (re-find #"\btrue\b" screen)))) => true
        "renders the show-inactive? boolean filter control as a toggle"
        (boolean (re-find #"\[[ x]\] Show inactive\?" screen)) => true
        "lists a seeded active account and hides an inactive one by default (Bob)"
        (and (boolean (re-find #"Alice" screen)) (not (boolean (re-find #"\bBob\b" screen)))) => true))))

(specification "report rendering (Inventory) — picker control + formatters"
  (log/with-merged-config {:min-level :error}
    (let [app (fresh-app!)
          ;; Preload the category picker-control options (client/main does this at startup).
          ctl (get-in (rc/component-options (rc/registry-key->class :tui-demo.ui.item-forms/InventoryReport))
                [:com.fulcrologic.rad.control/controls :tui-demo.ui.item-forms/category])
          _   (po/load-picker-options! app (rc/registry-key->class :tui-demo.ui.item-forms/InventoryReport) {} ctl)
          _   (scr/route-to! app :tui-demo.ui.item-forms/InventoryReport {})
          screen (wait-for app #"Bearing|Bolt|Bracket|Cog")]
      (assertions
        "renders the category-label column via the formatter (a seeded category appears)"
        (boolean (re-find #"Hardware|Tools|Fasteners" screen)) => true
        "renders the price column with the $ currency formatter"
        (boolean (re-find #"\$\d" screen)) => true
        "renders the category picker filter control (defaulting to All)"
        (boolean (re-find #"Category.*All" screen)) => true
        "lists a seeded catalog item"
        (boolean (re-find #"Bearing|Bolt|Bracket|Cog" screen)) => true))))

(specification "form rendering (Account) — enum picker, boolean toggle, address subform"
  (log/with-merged-config {:min-level :error}
    (let [app (fresh-app!)
          _   (scr/route-to! app :tui-demo.ui.account-forms/AccountList {})
          _   (wait-for app #"Alice")]
      ;; Tab to the first report row and open its edit form.
      (loop [n 0]
        (let [f (engine/current-focus app)]
          (when-not (or (and (keyword? f) (= "row" (namespace f))) (> n 30))
            (tui-app/step! app {:key :tab})
            (recur (inc n)))))
      (tui-app/step! app {:key :enter})
      (let [screen (wait-for app #"Edit Account")]
        (assertions
          "renders the edit-form title"
          (boolean (re-find #"Edit Account" screen)) => true
          "marks required fields with an asterisk (Name*)"
          (boolean (re-find #"Name\*" screen)) => true
          "renders the role :enum field as a picker button showing the current label"
          (boolean (re-find #"Superuser|Normal User" screen)) => true
          "renders the active? :boolean field as a [x]/[ ] toggle"
          (boolean (re-find #"\[[ x]\]" screen)) => true
          "renders the primary-address component subform with its labelled fields"
          (and (boolean (re-find #"Street" screen)) (boolean (re-find #"State" screen))) => true)))))
