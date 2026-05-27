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

(defn seed-txn
  "Returns the seed transaction: 2 accounts and 3 invoices with 1-2 line items each."
  []
  [(new-account "Alice" "alice@example.com")
   (new-account "Bob" "bob@example.com")
   (new-invoice "invoice-1" "2026-01-15" "Alice"
     [(new-line-item "Widget" 3 9.99M)
      (new-line-item "Gadget" 1 19.95M)])
   (new-invoice "invoice-2" "2026-02-20" "Bob"
     [(new-line-item "Sprocket" 5 2.50M)])
   (new-invoice "invoice-3" "2026-03-05" "Alice"
     [(new-line-item "Cog" 10 1.25M)
      (new-line-item "Bearing" 4 7.00M)])])

(defn fresh-conn
  "Creates a fresh Datascript connection with the demo schema and seeds it.
   Returns the seeded connection."
  []
  (let [conn (d/create-conn (driver/automatic-schema model/all-attributes :production))]
    (d/transact! conn (seed-txn))
    conn))
