(ns tui-demo.model.item
  "RAD attributes for the `item` (catalog product) entity. CLJC and babashka-safe.
   The `:item/all-items` source-attribute resolver lives server-side in
   `tui-demo.server.resolvers`."
  (:refer-clojure :exclude [name])
  (:require
    [com.fulcrologic.rad.attributes :refer [defattr]]
    [com.fulcrologic.rad.attributes-options :as ao]))

(defattr id :item/id :uuid
  {ao/identity? true
   ao/schema    :production})

(defattr category :item/category :ref
  {ao/target      :category/id
   ao/cardinality :one
   ao/identities  #{:item/id}
   ao/schema      :production})

(defattr name :item/name :string
  {ao/identities #{:item/id}
   ao/required?  true
   ao/schema     :production})

(defattr description :item/description :string
  {ao/identities #{:item/id}
   ao/schema     :production})

(defattr price :item/price :decimal
  {ao/identities #{:item/id}
   ao/schema     :production})

(defattr in-stock :item/in-stock :int
  {ao/identities #{:item/id}
   ao/schema     :production})

;; Virtual report-source attribute; resolver is server-side.
(defattr all-items :item/all-items :ref
  {ao/target    :item/id
   ao/pc-output [{:item/all-items [:item/id]}]})

(def attributes [id category name description price in-stock all-items])
