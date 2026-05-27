(ns tui-demo.server.parser
  "Pathom v2 parser for the demo server. Wires the RAD attribute plugin, form
   save/delete plugin, the Datascript driver plugin, and all resolvers (form
   mutations, driver-generated id-resolvers, and model `all-resolvers`)."
  (:require
    [com.fulcrologic.rad.attributes :as attr]
    [com.fulcrologic.rad.form :as form]
    [com.fulcrologic.rad.pathom :as pathom]
    [tui-demo.model.model :as model]
    [tui-demo.server.datascript-driver :as driver]
    [tui-demo.server.resolvers :as resolvers]))

(defonce ^{:doc "Holds the shared seeded Datascript connection for the demo, injected per-request."}
  connection (atom nil))

(defn set-connection!
  "Sets the shared Datascript `conn` used by the parser for all requests."
  [conn]
  (reset! connection conn))

(def parser
  "The Pathom 2 parser for the demo server."
  (pathom/new-parser {}
    [(attr/pathom-plugin model/all-attributes)
     (form/pathom-plugin (driver/wrap-save) (driver/wrap-delete))
     (driver/pathom-plugin (fn [_env] @connection))]
    [form/resolvers
     (driver/generate-resolvers model/all-attributes :production)
     resolvers/resolvers]))

(defn process-eql
  "Runs `eql` through the parser with a fresh env. The connection is taken from the
   shared `connection` atom by the driver plugin."
  [eql]
  (parser {} eql))
