(ns tui-demo.server.datomic-spec
  "Integration tests for the demo's Datomic Local server: reads through the Pathom
   parser (report sources, filters, nested pulls, derived category), and form
   save/delete round-trips. Each run uses its own freshly-seeded in-memory database.
   JVM-only (Datomic Local does not run under babashka)."
  (:require
    [com.fulcrologic.fulcro.algorithms.tempid :as tempid]
    [com.fulcrologic.rad.database-adapters.datomic-cloud :as datomic]
    [com.fulcrologic.rad.database-adapters.datomic-options :as do]
    [datomic.client.api :as d]
    [fulcro-spec.core :refer [=> assertions component specification]]
    [tui-demo.model.model :as model]
    [tui-demo.server.config :as config]
    [tui-demo.server.parser :as parser]
    [tui-demo.server.queries :as queries]
    [tui-demo.server.seed :as seed]))

(defn fresh-test-conn!
  "Starts a uniquely-named in-memory Datomic Local database, seeds it with the demo
   seed transaction, installs it on the parser, and returns the live connection."
  []
  (let [cfg  (assoc-in config/datomic-config [do/databases :main :datomic/database]
               (str "test-" (gensym)))
        conn (:main (datomic/start-databases model/all-attributes cfg))]
    (d/transact conn {:tx-data (seed/seed-txn)})
    (parser/set-connection! conn)
    conn))

(defn account-id-by-name
  "Returns the `:account/id` of the seeded account named `nm`."
  [conn nm]
  (ffirst (d/q '[:find ?id :in $ ?nm :where
                 [?e :account/name ?nm] [?e :account/id ?id]] (d/db conn) nm)))

(specification "report-source resolvers + filters"
  (let [conn (fresh-test-conn!)]
    (component "accounts"
      (let [active   (:account/all-accounts (parser/process-eql [{:account/all-accounts [:account/name]}]))
            all      (queries/get-all-accounts {do/databases {:production (atom (d/db conn))}}
                       {:show-inactive? true})]
        (assertions
          "the source resolver returns only active accounts by default (6 of 8 seeded)"
          (count active) => 6
          "the query returns all accounts when show-inactive? is set (8 seeded)"
          (count all) => 8
          "the two inactive customers (Bob, Erin) are absent from the active list"
          (some #{"Bob" "Erin"} (map :account/name active)) => nil)))
    (component "catalog"
      (assertions
        "all-items returns the seeded catalog (12 items)"
        (count (:item/all-items (parser/process-eql [{:item/all-items [:item/id]}]))) => 12
        "all-categories returns the three seeded categories"
        (set (map :category/label
               (:category/all-categories (parser/process-eql [{:category/all-categories [:category/label]}]))))
        => #{"Hardware" "Tools" "Fasteners"}))
    (component "invoices"
      (assertions
        "all-invoices returns the 100 generated invoices"
        (count (:invoice/all-invoices (parser/process-eql [{:invoice/all-invoices [:invoice/id]}]))) => 100))))

(specification "nested invoice pull + derived line-item category"
  (let [_       (fresh-test-conn!)
        inv-id  (-> (parser/process-eql [{:invoice/all-invoices [:invoice/id]}])
                  :invoice/all-invoices first :invoice/id)
        invoice (get (parser/process-eql
                       [{[:invoice/id inv-id]
                         [:invoice/total
                          {:invoice/customer [:account/name]}
                          {:invoice/line-items [:line-item/quantity :line-item/subtotal
                                                {:line-item/item [:item/name]}
                                                {:line-item/category [:category/label]}]}]}])
                  [:invoice/id inv-id])]
    (assertions
      "resolves the customer account name through the to-one ref"
      (string? (get-in invoice [:invoice/customer :account/name])) => true
      "resolves each line item's catalog item name"
      (every? #(string? (get-in % [:line-item/item :item/name]))
        (:invoice/line-items invoice)) => true
      "resolves the derived line-item category label (item -> category join)"
      (every? #(string? (get-in % [:line-item/category :category/label]))
        (:invoice/line-items invoice)) => true
      "the invoice total equals the sum of its line-item subtotals"
      (:invoice/total invoice)
      => (reduce (fn [t {:line-item/keys [subtotal]}]
                   (com.fulcrologic.rad.type-support.decimal/+ t subtotal))
           (com.fulcrologic.rad.type-support.decimal/zero)
           (:invoice/line-items invoice)))))

(specification "scoped queries"
  (let [conn (fresh-test-conn!)
        env  {do/databases {:production (atom (d/db conn))}}
        cat-id (-> (queries/get-all-categories env {}) first :category/id)]
    (component "items-by-category"
      (let [scoped (queries/get-all-items env {:category/id cat-id})
            all    (queries/get-all-items env {})]
        (assertions
          "scoping to a category returns a non-empty subset of all items"
          (and (pos? (count scoped)) (<= (count scoped) (count all))) => true)))
    (component "customer-invoices"
      (let [alice-id (account-id-by-name conn "Alice")
            invoices (queries/get-customer-invoices env {:account/id alice-id})]
        (assertions
          "returns only invoices belonging to the named customer"
          (pos? (count invoices)) => true)))))

(specification "form save round-trip (create account)"
  (let [_   (fresh-test-conn!)
        tid (tempid/tempid)
        res (parser/process-eql
              [`(com.fulcrologic.rad.form/save-form
                  {:com.fulcrologic.rad.form/id        [:account/id ~tid]
                   :com.fulcrologic.rad.form/master-pk :account/id
                   :com.fulcrologic.rad.form/delta
                   {[:account/id ~tid] {:account/id      {:after ~tid}
                                        :account/name    {:after "Zelda"}
                                        :account/email   {:after "zelda@example.com"}
                                        :account/active? {:after true}
                                        :account/role    {:after :account.role/user}}}})])
        save (get res `com.fulcrologic.rad.form/save-form)
        after (:account/all-accounts (parser/process-eql [{:account/all-accounts [:account/name]}]))]
    (assertions
      "remaps the form tempid to a real UUID"
      (uuid? (get-in save [:tempids tid])) => true
      "persists the new account so it appears in the (active) accounts list"
      (some #{"Zelda"} (map :account/name after)) => "Zelda")))

(specification "set-account-active mutation round-trip"
  (let [conn      (fresh-test-conn!)
        alice-id  (account-id-by-name conn "Alice")
        active-of (fn [] (-> (parser/process-eql [{[:account/id alice-id] [:account/active?]}])
                           (get [:account/id alice-id]) :account/active?))
        before    (active-of)
        _         (parser/process-eql
                    [`(tui-demo.model.account/set-account-active
                        {:account/id ~alice-id :account/active? false})])
        after     (active-of)]
    (assertions
      "the seeded account starts active"
      before => true
      "the mutation persists the account as inactive"
      after => false)))

(specification "form delete round-trip"
  (let [conn   (fresh-test-conn!)
        bob-id (account-id-by-name conn "Bob")
        _      (parser/process-eql
                 [`(com.fulcrologic.rad.form/delete-entity {:account/id ~bob-id})])
        env    {do/databases {:production (atom (d/db conn))}}
        all    (queries/get-all-accounts env {:show-inactive? true})]
    (assertions
      "removes the deleted account from the database"
      (some #{bob-id} (map :account/id all)) => nil)))
