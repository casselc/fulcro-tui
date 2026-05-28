(ns tui-demo.model.line-item
  "RAD attributes for the `line-item` entity. CLJC and babashka-safe. A line item
   references a catalog `:line-item/item` and carries the `:line-item/quoted-price`
   captured at sale time. `:line-item/category` is a *derived* (non-persisted) ref
   used by the form to filter the item picker; its resolver lives server-side in
   `tui-demo.server.resolvers`."
  (:require
    [com.fulcrologic.rad.attributes :refer [defattr]]
    [com.fulcrologic.rad.attributes-options :as ao]))

(defattr id :line-item/id :uuid
  {ao/identity? true
   ao/schema    :production})

;; Derived (not stored): resolved from the line item's :item -> :category server-side.
;; Used as a form pick-one to scope the :line-item/item picker by category.
(defattr category :line-item/category :ref
  {ao/target      :category/id
   ao/cardinality :one
   ao/pc-input    #{:line-item/id}
   ao/pc-output   [{:line-item/category [:category/id]}]})

(defattr item :line-item/item :ref
  {ao/target      :item/id
   ao/cardinality :one
   ao/identities  #{:line-item/id}
   ao/required?   true
   ao/schema      :production})

(defattr quantity :line-item/quantity :int
  {ao/identities #{:line-item/id}
   ao/required?  true
   ao/schema     :production})

(defattr quoted-price :line-item/quoted-price :decimal
  {ao/identities #{:line-item/id}
   ao/required?  true
   ao/schema     :production})

(defattr subtotal :line-item/subtotal :decimal
  {ao/identities #{:line-item/id}
   ao/read-only? true
   ao/schema     :production})

(def attributes [id category item quantity quoted-price subtotal])
