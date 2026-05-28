(ns tui-demo.server.parser
  "Pathom v2 parser for the demo server. Wires the RAD attribute plugin, the form
   save/delete plugin (backed by the Datomic adapter middleware), the datomic-cloud
   pathom plugin, and all resolvers (form mutations, RAD attribute resolvers,
   Datomic-generated id-resolvers, and the demo's source/derived resolvers)."
  (:require
    [com.fulcrologic.rad.attributes :as attr]
    [com.fulcrologic.rad.database-adapters.datomic-cloud :as datomic]
    [com.fulcrologic.rad.form :as form]
    [com.fulcrologic.rad.pathom :as pathom]
    [com.fulcrologic.rad.resolvers :as res]
    [com.fulcrologic.rad.type-support.date-time :as dt]
    [com.wsscode.pathom.core :as p]
    [tui-demo.model.model :as model]
    [tui-demo.server.resolvers :as resolvers]))

(defonce ^{:doc "Holds the shared seeded Datomic connection for the demo, injected per-request."}
  connection (atom nil))

(defn set-connection!
  "Sets the shared Datomic `conn` used by the parser for all requests."
  [conn]
  (reset! connection conn))

(def ^:private timezone-plugin
  "Wraps every parse in America/Los_Angeles so RAD `:date-at-noon` instants are
   interpreted in a stable zone (matching the seed data)."
  {::p/wrap-parser
   (fn [parser]
     (fn [env tx]
       (dt/with-timezone "America/Los_Angeles"
         (if (and (map? env) (seq tx)) (parser env tx) {}))))})

(def parser
  "The Pathom 2 parser for the demo server."
  (pathom/new-parser {}
    [(attr/pathom-plugin model/all-attributes)
     (form/pathom-plugin (datomic/wrap-datomic-save) (datomic/wrap-datomic-delete))
     (datomic/pathom-plugin (fn [_env] {:production @connection}))
     timezone-plugin]
    [form/resolvers
     (res/generate-resolvers model/all-attributes)
     (datomic/generate-resolvers model/all-attributes :production)
     resolvers/resolvers]))

(defn process-eql
  "Runs `eql` through the parser with a fresh env. The connection is taken from the
   shared `connection` atom by the datomic pathom plugin."
  [eql]
  (parser {} eql))
