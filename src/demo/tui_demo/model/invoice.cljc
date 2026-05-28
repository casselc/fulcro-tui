(ns tui-demo.model.invoice
  "RAD attributes for the `invoice` entity. CLJC and fully client-safe (loads under
   babashka): NO server/datomic dependency. The `:invoice/all-invoices`
   source-attribute resolver lives server-side in `tui-demo.server.resolvers`."
  (:require
    [com.fulcrologic.fulcro.raw.components :as rc]
    [com.fulcrologic.rad.attributes :refer [defattr]]
    [com.fulcrologic.rad.attributes-options :as ao]
    [com.fulcrologic.rad.form-options :as fo]
    [com.fulcrologic.rad.report-options :as ro]))

(defattr id :invoice/id :uuid
  {ao/identity? true
   ao/schema    :production})

(defattr date :invoice/date :instant
  {fo/field-style :date-at-noon
   ao/identities  #{:invoice/id}
   ao/required?   true
   ao/schema      :production})

(defattr customer :invoice/customer :ref
  {ao/target      :account/id
   ro/column-EQL {:invoice/customer (rc/get-query (rc/nc [:account/id :account/name] {:componentName :invoice/customer-column-query}))}
   ro/column-formatter (fn [_ v _ _] (:account/name v))
   ao/cardinality :one
   ao/identities #{:invoice/id}
   ao/required? true
   ao/schema :production})

(defattr line-items :invoice/line-items :ref
  {ao/target                                                       :line-item/id
   ao/cardinality :many
   ao/component?  true
   ao/identities  #{:invoice/id}
   ao/required?   true
   ao/schema      :production
   :com.fulcrologic.rad.database-adapters.datomic/attribute-schema {:db/isComponent true}})

(defattr total :invoice/total :decimal
  {ao/identities #{:invoice/id}
   ao/read-only? true
   ao/schema     :production})

;; Virtual report source attribute. The resolver is defined server-side
;; (tui-demo.server.resolvers) so this model ns stays client/babashka-safe.
(defattr all-invoices :invoice/all-invoices :ref
  {ao/target    :invoice/id
   ao/pc-output [{:invoice/all-invoices [:invoice/id]}]})

(def attributes [id date customer line-items total all-invoices])
