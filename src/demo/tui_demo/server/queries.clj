(ns tui-demo.server.queries
  "Database query functions backing the virtual `all-*` resolvers. Each returns a
   vector of idents by querying the Datascript db reachable from the pathom `env`."
  (:require
    [datascript.core :as d]
    [tui-demo.server.datascript-driver :as driver]))

(defn- env->db
  "Returns the current Datascript db value from the pathom `env`, or nil."
  [env]
  (some-> env driver/current-connection d/db))

(defn get-all-accounts
  "Returns all account idents (`{:account/id uuid}`) in the database from `env`."
  [env]
  (when-let [db (env->db env)]
    (mapv (fn [[id]] {:account/id id})
      (d/q '[:find ?uuid :where [?e :account/id ?uuid]] db))))

(defn get-all-invoices
  "Returns all invoice idents (`{:invoice/id uuid}`) in the database from `env`."
  [env]
  (when-let [db (env->db env)]
    (mapv (fn [[id]] {:invoice/id id})
      (d/q '[:find ?uuid :where [?e :invoice/id ?uuid]] db))))
