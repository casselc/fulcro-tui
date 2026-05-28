(ns tui-demo.model.account
  "RAD attributes for the `account` entity. CLJC and fully client-safe (loads under
   babashka): it has NO server/datomic dependency. The `:account/all-accounts` and
   `:account/invoices` source-attribute resolvers live server-side in
   `tui-demo.server.resolvers`."
  (:refer-clojure :exclude [name])
  (:require
    [clojure.string :as str]
    [com.fulcrologic.fulcro.mutations :refer [defmutation]]
    [com.fulcrologic.rad.attributes :refer [defattr]]
    [com.fulcrologic.rad.attributes-options :as ao]
    [com.fulcrologic.rad.form-options :as fo]))

(defattr id :account/id :uuid
  {ao/identity? true
   ao/schema    :production})

(defattr name :account/name :string
  {ao/identities #{:account/id}
   ao/required?  true
   ao/schema     :production})

(defattr email :account/email :string
  {ao/identities         #{:account/id}
   ao/required?          true
   ao/schema             :production
   fo/validation-message "Email must start with your lower-cased first name."
   ao/valid?             (fn [v props _]
                           (let [prefix (or (some-> props :account/name
                                              (str/split #"\s") first str/lower-case)
                                          "")]
                             (str/starts-with? (or v "") prefix)))})

(defattr active? :account/active? :boolean
  {ao/identities    #{:account/id}
   ao/schema        :production
   fo/default-value true})

(def account-roles
  "The selectable account roles: enum value -> human label."
  {:account.role/superuser "Superuser"
   :account.role/user      "Normal User"})

(defattr role :account/role :enum
  {ao/identities        #{:account/id}
   ao/schema            :production
   ao/enumerated-values (set (keys account-roles))
   ao/enumerated-labels account-roles})

(defattr primary-address :account/primary-address :ref
  {ao/target      :address/id
   ao/cardinality :one
   ao/identities  #{:account/id}
   ao/schema      :production
   ao/component?  true
   :com.fulcrologic.rad.database-adapters.datomic/attribute-schema {:db/isComponent true}})

(defattr addresses :account/addresses :ref
  {ao/target      :address/id
   ao/cardinality :many
   ao/identities  #{:account/id}
   ao/schema      :production
   ao/component?  true
   :com.fulcrologic.rad.database-adapters.datomic/attribute-schema {:db/isComponent true}})

;; Virtual report-source attributes. Their resolvers are defined server-side
;; (tui-demo.server.resolvers) so this model ns stays client/babashka-safe.
(defattr all-accounts :account/all-accounts :ref
  {ao/target    :account/id
   ao/pc-output [{:account/all-accounts [:account/id]}]})

(defattr account-invoices :account/invoices :ref
  {ao/target    :invoice/id
   ao/pc-output [{:account/invoices [:invoice/id]}]})

(def attributes
  [id name email active? role primary-address addresses all-accounts account-invoices])

(defmutation set-account-active
  "Sets an account's `:account/active?` flag. Optimistically updates the client, then persists to the
   server (handled by the matching Pathom mutation in `tui-demo.server.resolvers`)."
  [{:account/keys [id active?]}]
  (action [{:keys [state]}]
    (swap! state assoc-in [:account/id id :account/active?] active?))
  (remote [_] true))
