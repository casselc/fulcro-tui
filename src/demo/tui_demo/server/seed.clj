(ns tui-demo.server.seed
  "Builds and seeds an in-memory Datomic Local database for the demo. Follows the
   minimal-data builder pattern: `new-*` constructors take only the salient fields
   and accept arbitrary k/v overrides. Within a single transaction, string `:db/id`
   values act as tempids so entities can reference each other by name."
  (:require
    [cljc.java-time.local-time :as lt]
    [com.fulcrologic.rad.database-adapters.datomic-cloud :as datomic]
    [com.fulcrologic.rad.ids :refer [new-uuid]]
    [com.fulcrologic.rad.type-support.date-time :as dt]
    [com.fulcrologic.rad.type-support.decimal :as math]
    [datomic.client.api :as d]
    [tui-demo.model.model :as model]
    [tui-demo.server.config :as config]))

(defn date-str->inst
  "Converts an html date string (yyyy-MM-dd) to an inst at noon, America/Los_Angeles."
  [date-str]
  (dt/with-timezone "America/Los_Angeles"
    (dt/html-date->inst date-str lt/noon)))

(defn- cat-tempid [label] (str "cat-" label))
(defn- item-tempid [item-name] (str "item-" item-name))

(defn new-category
  "Builds a category tx map. Its `:db/id` tempid is `cat-<label>`."
  [label & {:as addl}]
  (merge {:db/id          (cat-tempid label)
          :category/id    (new-uuid)
          :category/label label}
    addl))

(defn new-item
  "Builds a catalog item tx map referencing the category tempid for `cat-label`.
   Its `:db/id` tempid is `item-<name>`."
  [item-name price cat-label in-stock & {:as addl}]
  (merge {:db/id           (item-tempid item-name)
          :item/id         (new-uuid)
          :item/name       item-name
          :item/description (str item-name " — a fine product.")
          :item/price      (math/numeric price)
          :item/in-stock   in-stock
          :item/category   (cat-tempid cat-label)}
    addl))

(defn new-account
  "Builds an account tx map with a primary address. `:db/id` tempid is `name`."
  [name email active? role & {:as addl}]
  (merge {:db/id                   name
          :account/id              (new-uuid)
          :account/name            name
          :account/email           email
          :account/active?         active?
          :account/role            role
          :account/primary-address {:address/id     (new-uuid)
                                     :address/street (str "100 " name " St")
                                     :address/city   "Springfield"
                                     :address/state  :address.state/CA
                                     :address/zip    "90210"}}
    addl))

(defn new-line-item
  "Builds a line-item tx map referencing the catalog item tempid for `item-name`.
   `:line-item/subtotal` is `quantity` * `quoted-price`."
  [item-name quantity price & {:as addl}]
  (let [p (math/numeric price)]
    (merge {:line-item/id           (new-uuid)
            :line-item/item         (item-tempid item-name)
            :line-item/quantity     quantity
            :line-item/quoted-price p
            :line-item/subtotal     (math/* quantity p)}
      addl)))

(defn new-invoice
  "Builds an invoice owned by `customer` (a tempid/lookup-ref) for `line-items`
   (a vector of line-item tx maps). `:invoice/total` is the sum of subtotals.
   `date-str` is an html date string (yyyy-MM-dd). `:db/id` tempid is `str-id`."
  [str-id date-str customer line-items & {:as addl}]
  (merge {:db/id              str-id
          :invoice/id         (new-uuid)
          :invoice/date       (date-str->inst date-str)
          :invoice/customer   customer
          :invoice/line-items line-items
          :invoice/total      (reduce (fn [t {:line-item/keys [subtotal]}] (math/+ t subtotal))
                                (math/zero)
                                line-items)}
    addl))

(def ^:private customers
  "Demo customer roster: `[name email active? role]`. `name` doubles as the account tempid."
  [["Alice" "alice@example.com" true  :account.role/superuser]
   ["Bob"   "bob@example.com"   false :account.role/user]
   ["Carol" "carol@example.com" true  :account.role/user]
   ["Dave"  "dave@example.com"  true  :account.role/user]
   ["Erin"  "erin@example.com"  false :account.role/user]
   ["Frank" "frank@example.com" true  :account.role/user]
   ["Grace" "grace@example.com" true  :account.role/user]
   ["Heidi" "heidi@example.com" true  :account.role/user]])

(def ^:private product-catalog
  "Demo catalog: `[name price category-label in-stock]`."
  [["Widget"   9.99M  "Hardware"  120]
   ["Gadget"   19.95M "Hardware"  60]
   ["Sprocket" 2.50M  "Hardware"  300]
   ["Cog"      1.25M  "Hardware"  450]
   ["Bearing"  7.00M  "Hardware"  90]
   ["Flange"   12.40M "Tools"     40]
   ["Grommet"  0.85M  "Fasteners" 800]
   ["Bracket"  4.75M  "Tools"     75]
   ["Washer"   0.35M  "Fasteners" 1200]
   ["Bolt"     0.60M  "Fasteners" 1000]
   ["Hinge"    3.20M  "Tools"     55]
   ["Pulley"   14.50M "Tools"     30]])

(def ^:private category-labels
  (vec (distinct (map #(nth % 2) product-catalog))))

(defn- generated-invoice
  "Builds invoice number `i` (1-based) deterministically: cycles through `customers`,
   picks a date spread across 2025, and carries 1-3 line items drawn from
   `product-catalog`. Line-item tempids are unique per invoice (`li-<i>-<j>`)."
  [i]
  (let [customer   (first (nth customers (mod i (count customers))))
        month      (inc (mod i 12))
        day        (inc (mod (* i 7) 27))
        date-str   (format "2025-%02d-%02d" month day)
        line-count (inc (mod i 3))
        items      (mapv (fn [j]
                           (let [[nm price _ _] (nth product-catalog (mod (+ i (* 3 j)) (count product-catalog)))
                                 qty            (inc (mod (+ i j) 5))]
                             (new-line-item nm qty price :db/id (str "li-" i "-" j))))
                     (range line-count))]
    (new-invoice (str "invoice-" i) date-str customer items)))

(defn seed-txn
  "Returns the seed transaction: categories, catalog items, the `customers` roster,
   and 100 generated invoices (1-3 line items each) — enough volume to exercise
   report pagination and viewport scrolling."
  []
  (-> []
    (into (map new-category) category-labels)
    (into (map (fn [[nm price cat stock]] (new-item nm price cat stock))) product-catalog)
    (into (map (fn [[nm email active? role]] (new-account nm email active? role))) customers)
    (into (map generated-invoice) (range 1 101))))

(defn fresh-conn
  "Starts a fresh in-memory Datomic Local database (auto schema from the model),
   seeds it, and returns the live connection."
  []
  (let [conn (:main (datomic/start-databases model/all-attributes config/datomic-config))]
    (d/transact conn {:tx-data (seed-txn)})
    conn))
