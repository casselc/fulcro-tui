(ns tui-demo.server.seed
  "Builds and seeds an in-memory Datascript database for the demo. Follows the
   minimal-data builder pattern: `new-*` constructors take only the salient
   fields and accept arbitrary k/v overrides; `:db/id` is a readable string tempid."
  (:require
    [com.fulcrologic.rad.ids :refer [new-uuid]]
    [com.fulcrologic.rad.type-support.date-time :as dt]
    [com.fulcrologic.rad.type-support.decimal :as math]
    [cljc.java-time.local-time :as lt]
    [datascript.core :as d]
    [tui-demo.model.model :as model]
    [tui-demo.server.datascript-driver :as driver]))

(defn date-str->inst
  "Converts an html date string (yyyy-MM-dd) to an inst at noon in the given timezone."
  [date-str]
  (dt/with-timezone "America/Los_Angeles"
    (dt/html-date->inst date-str lt/noon)))

(defn new-account
  "Builds an account. `:db/id` tempid is `name`. `name` doubles as a readable id."
  [name email & {:as addl}]
  (merge
    {:db/id         name
     :account/id    (new-uuid)
     :account/name  name
     :account/email email}
    addl))

(defn new-line-item
  "Builds a line item with computed `:line-item/subtotal` (= `quantity` * `unit-price`).
   `:db/id` tempid is `description`."
  [description quantity unit-price & {:as addl}]
  (let [price (math/numeric unit-price)]
    (merge
      {:db/id              description
       :line-item/id       (new-uuid)
       :line-item/description description
       :line-item/quantity quantity
       :line-item/unit-price price
       :line-item/subtotal (math/* quantity price)}
      addl)))

(defn new-invoice
  "Builds an invoice owned by `customer` (a tempid/lookup-ref) for the given `line-items`
   (a vector of line-item tx maps). `:invoice/total` is the sum of subtotals. `date`
   is an html date string (yyyy-MM-dd). `:db/id` tempid is `str-id`."
  [str-id date-str customer line-items & {:as addl}]
  (merge
    {:db/id              str-id
     :invoice/id         (new-uuid)
     :invoice/date       (date-str->inst date-str)
     :invoice/customer   customer
     :invoice/line-items line-items
     :invoice/total      (reduce (fn [t {:line-item/keys [subtotal]}] (math/+ t subtotal))
                           (math/zero)
                           line-items)}
    addl))

(def ^:private customers
  "The demo customer roster: `[name email]` pairs. The `name` doubles as the account tempid."
  [["Alice" "alice@example.com"]
   ["Bob" "bob@example.com"]
   ["Carol" "carol@example.com"]
   ["Dave" "dave@example.com"]
   ["Erin" "erin@example.com"]
   ["Frank" "frank@example.com"]
   ["Grace" "grace@example.com"]
   ["Heidi" "heidi@example.com"]])

(def ^:private product-catalog
  "The demo product roster: `[description unit-price]` pairs drawn from when generating line items."
  [["Widget" 9.99M] ["Gadget" 19.95M] ["Sprocket" 2.50M] ["Cog" 1.25M]
   ["Bearing" 7.00M] ["Flange" 12.40M] ["Grommet" 0.85M] ["Bracket" 4.75M]
   ["Washer" 0.35M] ["Bolt" 0.60M] ["Hinge" 3.20M] ["Pulley" 14.50M]])

(defn- generated-invoice
  "Builds invoice number `i` (1-based) deterministically: it cycles through `customers`, picks a date
   spread across 2025, and carries 1-3 line items drawn from `product-catalog`. Line-item tempids are
   made unique per invoice (`li-<i>-<j>`) so they never collide across invoices in one transaction."
  [i]
  (let [customer   (first (nth customers (mod i (count customers))))
        month      (inc (mod i 12))
        day        (inc (mod (* i 7) 27))
        date-str   (format "2025-%02d-%02d" month day)
        line-count (inc (mod i 3))
        items      (mapv (fn [j]
                           (let [[desc price] (nth product-catalog (mod (+ i (* 3 j)) (count product-catalog)))
                                 qty           (inc (mod (+ i j) 5))]
                             (new-line-item desc qty price :db/id (str "li-" i "-" j))))
                     (range line-count))]
    (new-invoice (str "invoice-" i) date-str customer items)))

(defn seed-txn
  "Returns the seed transaction: the full `customers` roster plus 100 generated invoices (1-3 line
   items each) — enough volume to exercise report pagination and viewport scrolling."
  []
  (into (mapv (fn [[name email]] (new-account name email)) customers)
    (map generated-invoice)
    (range 1 101)))

(defn fresh-conn
  "Creates a fresh Datascript connection with the demo schema and seeds it.
   Returns the seeded connection."
  []
  (let [conn (d/create-conn (driver/automatic-schema model/all-attributes :production))]
    (d/transact! conn (seed-txn))
    conn))
