(ns tui-demo.model.line-item
  "RAD attributes for the `line-item` entity. CLJC and babashka-safe."
  (:require
    [com.fulcrologic.rad.attributes :refer [defattr]]
    [com.fulcrologic.rad.attributes-options :as ao]))

(defattr id :line-item/id :uuid
  {ao/identity? true
   ao/schema    :production})

(defattr description :line-item/description :string
  {ao/identities #{:line-item/id}
   ao/required?  true
   ao/schema     :production})

(defattr quantity :line-item/quantity :int
  {ao/identities #{:line-item/id}
   ao/required?  true
   ao/schema     :production})

(defattr unit-price :line-item/unit-price :decimal
  {ao/identities #{:line-item/id}
   ao/required?  true
   ao/schema     :production})

(defattr subtotal :line-item/subtotal :decimal
  {ao/identities #{:line-item/id}
   ao/read-only? true
   ao/schema     :production})

(def attributes [id description quantity unit-price subtotal])
