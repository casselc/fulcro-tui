(ns tui-demo.server.datascript-driver
  "A hand-ported RAD database adapter for an in-memory Datascript database (JVM).

   Ported from the artlab CLJS datascript driver and mirrors the contract of the
   fulcro-rad Datomic adapter: `automatic-schema`, `generate-resolvers`,
   `pathom-plugin`, `wrap-save`, `wrap-delete`, and `delta->txn`.

   RAD attribute options consulted here:
     * `::attr/schema`      - selects which attributes belong in this DB's schema.
     * `::attr/identity?`   - marks the primary key; becomes `:db.unique/identity`.
     * `::attr/identities`  - groups non-id attributes under their owning id key(s).
     * `::attr/type`        - mapped to a Datascript `:db/valueType` (only `:ref` matters).
     * `::attr/cardinality` - `:many` becomes `:db.cardinality/many`.
     * `::attr/target`      - the id key of a ref attribute's target entity.
     * `::attr/qualified-key` - the keyword used as the Datascript attribute."
  (:require
    [clojure.set :as set]
    [clojure.walk :as walk]
    [com.fulcrologic.fulcro.algorithms.do-not-use :refer [deep-merge]]
    [com.fulcrologic.fulcro.algorithms.tempid :as tempid]
    [com.fulcrologic.guardrails.core :refer [=> >defn ?]]
    [com.fulcrologic.rad.attributes :as attr]
    [com.fulcrologic.rad.authorization :as auth]
    [com.fulcrologic.rad.form :as form]
    [com.fulcrologic.rad.ids :refer [new-uuid]]
    [com.fulcrologic.rad.type-support.decimal :as math]
    [datascript.core :as d]
    [edn-query-language.core :as eql]
    [taoensso.encore :as enc]
    [taoensso.timbre :as log]))

;; ----------------------------------------------------------------------------
;; Connection access
;; ----------------------------------------------------------------------------

(defn current-connection
  "Returns the Datascript connection that `pathom-plugin` injected into `env`."
  [env]
  (:datascript/connection env))

;; ----------------------------------------------------------------------------
;; Schema generation
;; ----------------------------------------------------------------------------

(def type->datascript-value-type
  "Maps RAD attribute types that require a Datascript `:db/valueType`. Only `:ref`
   is meaningful to Datascript; scalar values are stored untyped."
  {:ref :db.type/ref})

(defn- attribute-schema
  "Builds the Datascript schema map for the given `attributes` (already filtered to a single schema)."
  [attributes]
  (reduce
    (fn [acc {::attr/keys [identity? type qualified-key cardinality]}]
      (let [datascript-type (get type->datascript-value-type type)
            schema          (cond-> {}
                              (= :many cardinality) (assoc :db/cardinality :db.cardinality/many)
                              datascript-type (assoc :db/valueType datascript-type)
                              identity? (assoc :db/unique :db.unique/identity))]
        (if (seq schema)
          (assoc acc qualified-key schema)
          acc)))
    {}
    attributes))

(>defn automatic-schema
  "Returns a Datascript schema map derived from the RAD `attributes` whose `::attr/schema`
   equals `schema-name`."
  [attributes schema-name]
  [::attr/attributes keyword? => map?]
  (let [attributes (filterv #(= schema-name (::attr/schema %)) attributes)]
    (when (empty? attributes)
      (log/warn "Automatic schema requested, but no attributes matched schema" schema-name))
    (attribute-schema attributes)))

;; ----------------------------------------------------------------------------
;; Pull-query <-> Pathom-query translation (UUID ids stay as-is in Datascript,
;; so there is no :db/id remapping needed as there is for Datomic native ids).
;; ----------------------------------------------------------------------------

(defn pull-many
  "Like `d/pull` but for a collection of lookup-ref `ids`. Preserves the order of `ids`."
  [db pull-spec ids]
  (let [sort-kw   (ffirst ids)
        pull-spec (if (some #{sort-kw} pull-spec) pull-spec (conj pull-spec sort-kw))
        order-map (into {} (map-indexed (fn [i [_ v]] [v i])) ids)]
    (->> (d/q '[:find (pull ?e pattern)
                :in $ [?e ...] pattern]
            db ids pull-spec)
      (mapv first)
      (sort-by #(order-map (get % sort-kw)))
      vec)))

;; ----------------------------------------------------------------------------
;; Delta -> Datascript transaction
;; ----------------------------------------------------------------------------

(def keys-in-delta
  (fn keys-in-delta [delta]
    (let [id-keys  (into #{} (map first) (keys delta))
          all-keys (into id-keys (mapcat keys) (vals delta))]
      all-keys)))

(defn schemas-for-delta
  "Returns the set of `::attr/schema`s referenced by the keys in `delta`."
  [{::attr/keys [key->attribute]} delta]
  (into #{}
    (keep #(-> % key->attribute ::attr/schema))
    (keys-in-delta delta)))

(defn ref?
  "Returns true if `k` is a RAD `:ref` attribute."
  [{::attr/keys [key->attribute]} k]
  (= :ref (some-> k key->attribute ::attr/type)))

(defn to-one?
  "Returns true if `k` is a to-one (non-many) attribute."
  [{::attr/keys [key->attribute]} k]
  (not (boolean (some-> k key->attribute (attr/to-many?)))))

(defn schema-value?
  "Returns true if `k` belongs to `target-schema` and is not an identity attribute."
  [{::attr/keys [key->attribute]} target-schema k]
  (let [{::attr/keys [schema identity?]} (key->attribute k)]
    (and (= schema target-schema) (not identity?))))

(defn tempid->intermediate-id
  "Returns a map from each Fulcro tempid in `delta` to a stable string usable as a Datascript tempid."
  [_env delta]
  (let [tempids (volatile! #{})]
    (walk/prewalk
      (fn [e] (when (tempid/tempid? e) (vswap! tempids conj e)) e)
      delta)
    (into {} (map (fn [t] [t (str (.-id t))])) @tempids)))

(defn uuid-ident?
  "Returns true when the id in `ident` uses a UUID-typed RAD identity attribute."
  [{::attr/keys [key->attribute]} ident]
  (= :uuid (some-> ident first key->attribute ::attr/type)))

(defn failsafe-id
  "Returns a transaction-safe entity reference for `ident`. Tempids become their
   stable string form, keywords pass through, everything else becomes a lookup ref."
  [_env ident]
  (if (keyword? ident)
    ident
    (let [[_ id] ident]
      (if (tempid/tempid? id)
        (str (.-id id))
        ident))))

(defn fix-numerics
  "Coerces `v` to a value Datascript can store based on the attribute `type`."
  [{::attr/keys [type]} v]
  (case type
    :decimal (math/numeric v)
    :double (double v)
    :float (double v)
    :int (long v)
    :long (long v)
    v))

(defn tx-value
  "Converts `v` to a transaction-safe value for attribute `k` based on its type/cardinality."
  [{::attr/keys [key->attribute] :as env} k v]
  (if (ref? env k)
    (failsafe-id env v)
    (fix-numerics (key->attribute k) v)))

(defn to-one-txn
  "Returns the add/retract tx forms for all to-one, non-identity values of `schema` in `delta`."
  [env schema delta]
  (into []
    (mapcat
      (fn [[ident entity-delta]]
        (reduce
          (fn [tx [k {:keys [before after]}]]
            (if (and (schema-value? env schema k) (to-one? env k))
              (cond
                (some? after) (conj tx [:db/add (failsafe-id env ident) k (tx-value env k after)])
                (some? before) (conj tx [:db/retract (failsafe-id env ident) k (tx-value env k before)])
                :else tx)
              tx))
          []
          entity-delta)))
    delta))

(defn to-many-txn
  "Returns the add/retract tx forms for all to-many, non-identity values of `schema` in `delta`."
  [env schema delta]
  (into []
    (mapcat
      (fn [[ident entity-delta]]
        (reduce
          (fn [tx [k {:keys [before after]}]]
            (if (and (schema-value? env schema k) (not (to-one? env k)))
              (let [before  (into #{} (map (fn [v] (tx-value env k v))) before)
                    after   (into #{} (map (fn [v] (tx-value env k v))) after)
                    adds    (mapv (fn [v] [:db/add (failsafe-id env ident) k v])
                              (set/difference after before))
                    removes (mapv (fn [v] [:db/retract (failsafe-id env ident) k v])
                              (set/difference before after))]
                (into tx (concat adds removes)))
              tx))
          []
          entity-delta)))
    delta))

(defn generate-next-id
  "Generates a UUID for the new entity whose id attribute is `k`. If a Fulcro tempid is
   given as `suggested-id`, the UUID embedded in the tempid is reused."
  ([env k] (generate-next-id env k (new-uuid)))
  ([{::attr/keys [key->attribute]} k suggested-id]
   (let [{::attr/keys [type]} (key->attribute k)]
     (if (= :uuid type)
       (cond
         (tempid/tempid? suggested-id) (.-id suggested-id)
         (uuid? suggested-id) suggested-id
         :else (new-uuid))
       (throw (ex-info "Cannot generate an ID for non-UUID identity attribute" {:attribute k}))))))

(defn tempids->generated-ids
  "Returns a map from each new-entity Fulcro tempid in `delta` to its freshly generated UUID."
  [env delta]
  (into {}
    (keep (fn [[k id]]
            (when (tempid/tempid? id)
              [id (generate-next-id env k id)])))
    (keys delta)))

(>defn delta->txn
  "Converts a RAD form `delta` for `schema` into a Datascript transaction.

   Returns a map with:
     * `:tempid->string`       - Fulcro tempid -> intermediate string tempid (for `d/transact!` result lookup).
     * `:tempid->generated-id` - Fulcro tempid -> the real UUID assigned to the new entity.
     * `:txn`                  - the vector of `[:db/add ...]`/`[:db/retract ...]` forms."
  [env schema delta]
  [map? keyword? map? => map?]
  (let [tempid->string       (tempid->intermediate-id env delta)
        tempid->generated-id (tempids->generated-ids env delta)
        new-id-txn           (into []
                               (keep (fn [[k id :as ident]]
                                       (when (and (tempid/tempid? id) (uuid-ident? env ident))
                                         [:db/add (tempid->string id) k (tempid->generated-id id)])))
                               (keys delta))]
    {:tempid->string       tempid->string
     :tempid->generated-id tempid->generated-id
     :txn                  (into new-id-txn
                             cat
                             [(to-one-txn env schema delta)
                              (to-many-txn env schema delta)])}))

;; ----------------------------------------------------------------------------
;; Resolver generation (id-resolvers for each entity, Pathom 2)
;; ----------------------------------------------------------------------------

(defn entity-query
  "Pulls the `::default-query` for the entity/entities identified by `input` against the
   db reachable from `env`. `input` may be a single map or a sequence of maps containing
   the id attribute value(s)."
  [{:keys       [::id-attribute ::default-query]
    ::attr/keys [key->attribute] :as env}
   input]
  (let [{::attr/keys [qualified-key]} id-attribute
        one? (not (sequential? input))]
    (enc/if-let [conn  (current-connection env)
                 db    (d/db conn)
                 query (or default-query '[*])
                 ids   (if one?
                         [(get input qualified-key)]
                         (into [] (keep #(get % qualified-key)) input))
                 ids   (mapv (fn [id] [qualified-key id]) ids)]
      (let [result (pull-many db query ids)]
        (if one? (first result) result))
      (do
        (log/error "Unable to complete entity-query for" qualified-key)
        nil))))

(defn id-resolver
  "Builds a Pathom 2 resolver from the entity's `id-attribute` to its `output-attributes`."
  [{::attr/keys [qualified-key]
    :keys       [::attr/schema :com.wsscode.pathom.connect/transform] :as id-attribute}
   output-attributes]
  (enc/if-let [_       id-attribute
               outputs (attr/attributes->eql output-attributes)]
    (let [resolve-sym (symbol (namespace qualified-key) (str (name qualified-key) "-resolver"))
          resolve-fn  (fn [{::attr/keys [key->attribute] :as env} input]
                        (->> (entity-query
                               (assoc env
                                 ::attr/schema schema
                                 ::id-attribute id-attribute
                                 ::default-query outputs)
                               input)
                          (auth/redact env)))]
      (cond-> {:com.wsscode.pathom.connect/sym     resolve-sym
               :com.wsscode.pathom.connect/output  outputs
               :com.wsscode.pathom.connect/batch?  true
               :com.wsscode.pathom.connect/input   #{qualified-key}
               :com.wsscode.pathom.connect/resolve (fn [env input]
                                                     (resolve-fn (assoc env :com.wsscode.pathom.connect/sym resolve-sym) input))}
        transform transform))
    (do
      (log/error "Unable to generate id-resolver for" qualified-key)
      nil)))

(>defn generate-resolvers
  "Generates the Pathom 2 id-resolvers for every entity owning attributes in `schema`.
   Mirrors the Datomic adapter's `generate-resolvers`."
  [attributes schema]
  [::attr/attributes keyword? => vector?]
  (let [attributes            (filterv #(= schema (::attr/schema %)) attributes)
        key->attribute        (attr/attribute-map attributes)
        entity-id->attributes (group-by ::k
                                (mapcat (fn [attribute]
                                          (mapv (fn [id-key] (assoc attribute ::k id-key))
                                            (get attribute ::attr/identities)))
                                  attributes))]
    (reduce-kv
      (fn [result k v]
        (enc/if-let [id-attr  (key->attribute k)
                     resolver (id-resolver id-attr v)]
          (conj result resolver)
          (do
            (log/error "Internal error generating resolver for ID key" k)
            result)))
      []
      entity-id->attributes)))

;; ----------------------------------------------------------------------------
;; Pathom plugin (injects the connection per-request)
;; ----------------------------------------------------------------------------

(defn pathom-plugin
  "Pathom 2 plugin that injects a Datascript connection into the env under
   `:datascript/connection`. `connection-factory` is a `(fn [env] conn)`."
  [connection-factory]
  {:com.wsscode.pathom.core/wrap-parser
   (fn env-wrap-wrap-parser [parser]
     (fn env-wrap-wrap-internal [env tx]
       (parser (assoc env :datascript/connection (connection-factory env)) tx)))})

;; ----------------------------------------------------------------------------
;; Save / Delete
;; ----------------------------------------------------------------------------

(defn save-form!
  "Applies the form `delta` in `save-params` to the Datascript database in `env`.
   Returns `{:tempids {fulcro-tempid real-uuid}}`."
  [env {::form/keys [delta]}]
  (let [connection (current-connection env)
        schemas    (schemas-for-delta env delta)
        result     (atom {:tempids {}})]
    (doseq [schema schemas
            :let [{:keys [tempid->string tempid->generated-id txn]} (delta->txn env schema delta)]]
      (log/debug "Saving form delta on schema" schema "txn" txn)
      (if (and connection (seq txn))
        (try
          (let [{:keys [tempids]} (d/transact! connection txn)
                tempid->real-id (into {}
                                  (map (fn [tempid]
                                         [tempid (get tempid->generated-id tempid
                                                   (get tempids (tempid->string tempid)))]))
                                  (keys tempid->string))]
            (swap! result update :tempids merge tempid->real-id))
          (catch Throwable e
            (log/error e "Transaction failed!")
            (throw e)))
        (log/error "Unable to save form: missing connection or empty txn.")))
    @result))

(defn delete-entity!
  "Deletes the entity identified by the (single) ident in `params` from Datascript."
  [{::attr/keys [key->attribute] :as env} params]
  (enc/if-let [pk         (ffirst params)
               id         (get params pk)
               ident      [pk id]
               _          (key->attribute pk)
               connection (current-connection env)]
    (do
      (log/info "Deleting" ident)
      (d/transact! connection [[:db.fn/retractEntity ident]])
      {})
    (do
      (log/warn "Datascript adapter failed to delete" params)
      {})))

(defn wrap-save
  "Form save middleware that performs Datascript saves."
  ([]
   (fn [{::form/keys [params] :as pathom-env}]
     (save-form! pathom-env params)))
  ([handler]
   (fn [{::form/keys [params] :as pathom-env}]
     (let [save-result    (save-form! pathom-env params)
           handler-result (handler pathom-env)]
       (deep-merge save-result handler-result)))))

(defn wrap-delete
  "Form delete middleware that performs Datascript deletes."
  ([]
   (fn [{::form/keys [params] :as pathom-env}]
     (delete-entity! pathom-env params)))
  ([handler]
   (fn [{::form/keys [params] :as pathom-env}]
     (let [handler-result (handler pathom-env)
           local-result   (delete-entity! pathom-env params)]
       (deep-merge handler-result local-result)))))
