(ns tui-demo.model.account
  "RAD attributes for the `account` entity. CLJC and fully client-safe (loads under
   babashka): it has NO server/datascript dependency. The `:account/all-accounts`
   source-attribute resolver lives server-side in `tui-demo.server.resolvers`."
  (:refer-clojure :exclude [name])
  (:require
    [com.fulcrologic.rad.attributes :refer [defattr]]
    [com.fulcrologic.rad.attributes-options :as ao]))

(defattr id :account/id :uuid
  {ao/identity? true
   ao/schema    :production})

(defattr name :account/name :string
  {ao/identities #{:account/id}
   ao/required?  true
   ao/schema     :production})

(defattr email :account/email :string
  {ao/identities #{:account/id}
   ao/required?  true
   ao/schema     :production})

;; Virtual report source attribute. The resolver is defined server-side
;; (tui-demo.server.resolvers) so this model ns stays client/babashka-safe.
(defattr all-accounts :account/all-accounts :ref
  {ao/target    :account/id
   ao/pc-output [{:account/all-accounts [:account/id]}]})

(def attributes [id name email all-accounts])
