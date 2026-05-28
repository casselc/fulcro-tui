(ns tui-demo.model.model-spec
  "Behavioral tests for the demo's CLJC model: the account email validator, the
   aggregate attribute set, and the enumerated value sets. These are pure and
   babashka-safe (no datomic)."
  (:require
    [com.fulcrologic.rad.attributes-options :as ao]
    [fulcro-spec.core :refer [=> assertions component specification]]
    [tui-demo.model.account :as account]
    [tui-demo.model.address :as address]
    [tui-demo.model.model :as model]))

(def email-valid?
  "The account/email `ao/valid?` predicate: (fn [value props key] -> boolean)."
  (get account/email ao/valid?))

(specification "account/email ao/valid?"
  (assertions
    "accepts an email whose local part starts with the lower-cased first name"
    (email-valid? "alice@example.com" {:account/name "Alice"} :account/email) => true
    "accepts when the name has multiple words (uses the first word only)"
    (email-valid? "alice@example.com" {:account/name "Alice Smith"} :account/email) => true
    "rejects an email that does not start with the first name"
    (email-valid? "bob@example.com" {:account/name "Alice"} :account/email) => false
    "lower-cases the name before comparing"
    (email-valid? "alice@example.com" {:account/name "ALICE"} :account/email) => true
    "treats a missing name as an empty prefix (any value is valid)"
    (email-valid? "anything@example.com" {} :account/email) => true
    "rejects a nil value when a prefix is required"
    (email-valid? nil {:account/name "Alice"} :account/email) => false))

(specification "model/all-attributes aggregation"
  (let [ks (set (map ao/qualified-key model/all-attributes))]
    (assertions
      "includes the account identity and new account fields"
      (every? ks [:account/id :account/active? :account/role
                  :account/primary-address :account/addresses]) => true
      "includes the address, category, and item entities"
      (every? ks [:address/id :address/state :category/id :category/label
                  :item/id :item/category :item/price]) => true
      "includes the line-item item reference and derived category"
      (every? ks [:line-item/item :line-item/category :line-item/quoted-price]) => true
      "exposes a key->attribute lookup keyed by qualified-key"
      (ao/qualified-key (get model/key->attribute :invoice/total)) => :invoice/total)))

(specification "enumerated value sets"
  (component "account role"
    (assertions
      "offers superuser and normal-user roles"
      (get account/role ao/enumerated-values) => #{:account.role/superuser :account.role/user}
      "labels the superuser role"
      (get account/account-roles :account.role/superuser) => "Superuser"))
  (component "address state"
    (assertions
      "includes California among the state enum values"
      (contains? (get address/state ao/enumerated-values) :address.state/CA) => true
      "labels states with their two-letter code"
      (get (get address/state ao/enumerated-labels) :address.state/CA) => "CA")))
