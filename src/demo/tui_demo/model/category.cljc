(ns tui-demo.model.category
  "RAD attributes for the `category` entity. CLJC and babashka-safe. The
   `:category/all-categories` source-attribute resolver lives server-side in
   `tui-demo.server.resolvers`."
  (:require
    [com.fulcrologic.rad.attributes :refer [defattr]]
    [com.fulcrologic.rad.attributes-options :as ao]))

(defattr id :category/id :uuid
  {ao/identity? true
   ao/schema    :production})

(defattr label :category/label :string
  {ao/identities #{:category/id}
   ao/required?  true
   ao/schema     :production})

;; Virtual report-source attribute; resolver is server-side.
(defattr all-categories :category/all-categories :ref
  {ao/target    :category/id
   ao/pc-output [{:category/all-categories [:category/id]}]})

(def attributes [id label all-categories])
