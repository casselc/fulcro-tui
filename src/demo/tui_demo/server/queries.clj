(ns tui-demo.server.queries
  "Datomic query functions backing the demo's virtual/derived resolvers. Each reads
   the current db value from the pathom `env` (the datomic-cloud pathom-plugin places
   a db atom under `do/databases`) and returns idents or scalar values."
  (:require
    [com.fulcrologic.rad.database-adapters.datomic-options :as do]
    [datomic.client.api :as d]))

(defn- env->db
  "Returns the current `:production` Datomic db value from the pathom `env`, or nil."
  [env]
  (some-> env (get-in [do/databases :production]) deref))

(defn get-all-accounts
  "Returns account idents. When `show-inactive?` is falsey, only active accounts."
  [env {:keys [show-inactive?]}]
  (when-let [db (env->db env)]
    (let [rows (if show-inactive?
                 (d/q '[:find ?uuid :where [?e :account/id ?uuid]] db)
                 (d/q '[:find ?uuid :where
                        [?e :account/active? true]
                        [?e :account/id ?uuid]] db))]
      (mapv (fn [[id]] {:account/id id}) rows))))

(defn get-all-invoices
  "Returns all invoice idents."
  [env _params]
  (when-let [db (env->db env)]
    (mapv (fn [[id]] {:invoice/id id})
      (d/q '[:find ?uuid :where [?e :invoice/id ?uuid]] db))))

(defn get-customer-invoices
  "Returns invoice idents for the account with `:account/id` `id`."
  [env {:account/keys [id]}]
  (when-let [db (env->db env)]
    (mapv (fn [[iid]] {:invoice/id iid})
      (d/q '[:find ?uuid :in $ ?cid :where
             [?c :account/id ?cid]
             [?i :invoice/customer ?c]
             [?i :invoice/id ?uuid]] db id))))

(defn get-all-categories
  "Returns all category idents."
  [env _params]
  (when-let [db (env->db env)]
    (mapv (fn [[id]] {:category/id id})
      (d/q '[:find ?uuid :where [?e :category/id ?uuid]] db))))

(defn get-all-items
  "Returns item idents, optionally scoped to category `:category/id` `id`."
  [env {:category/keys [id]}]
  (when-let [db (env->db env)]
    (let [rows (if id
                 (d/q '[:find ?uuid :in $ ?cid :where
                        [?c :category/id ?cid]
                        [?i :item/category ?c]
                        [?i :item/id ?uuid]] db id)
                 (d/q '[:find ?uuid :where [?e :item/id ?uuid]] db))]
      (mapv (fn [[iid]] {:item/id iid}) rows))))

(defn get-line-item-category
  "Returns the `:category/id` of the catalog item referenced by line item `id`, or nil.
   Backs the derived `:line-item/category` attribute."
  [env id]
  (when-let [db (env->db env)]
    (ffirst
      (d/q '[:find ?catid :in $ ?liid :where
             [?li :line-item/id ?liid]
             [?li :line-item/item ?item]
             [?item :item/category ?c]
             [?c :category/id ?catid]] db id))))
