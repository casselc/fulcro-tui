(ns tui-demo.ui.line-item-form
  "RAD (statechart) subform for a single invoice line item. Edited only as a child
   of `tui-demo.ui.invoice-form/InvoiceForm`.

   The two pick-one references are *dependent*: choosing a `:line-item/category`
   reloads the `:line-item/item` picker's options (scoped to that category) and
   clears any previously-chosen item; choosing an `:line-item/item` copies that
   catalog item's price into `:line-item/quoted-price`. `:line-item/subtotal` is
   derived (quantity × quoted-price)."
  (:require
    [com.fulcrologic.fulcro.raw.components :as rc]
    [com.fulcrologic.rad.form-options :as fo]
    [com.fulcrologic.rad.picker-options :as po]
    [com.fulcrologic.rad.statechart.form :refer [defsc-form]]
    [com.fulcrologic.rad.statechart.form-options :as sfo]
    [com.fulcrologic.rad.type-support.decimal :as math]
    [com.fulcrologic.statecharts.integration.fulcro.operations :as fops]
    [tui-demo.model.category :as category]
    [tui-demo.model.item :as item]
    [tui-demo.model.line-item :as line-item]))

(def CategoryQuery
  "Normalizing query component for category picker options (loads into `:category/id`)."
  (rc/nc [:category/id :category/label] {:componentName ::CategoryQuery}))

(def ItemQuery
  "Normalizing query component for item picker options (loads into `:item/id`)."
  (rc/nc [:item/id :item/name :item/price {:item/category [:category/id]}]
    {:componentName ::ItemQuery}))

(defn- category-id
  "Extracts the `:category/id` uuid from a `:line-item/category` value. The value is a denormalized
   `{:category/id id}` map when read from a form instance's props, but a bare `[:category/id id]` ident
   when read from the raw state-map (e.g. inside an `:on-change` trigger). Both must yield the same id
   so the dependent item picker's cache-key/query-parameters match between the trigger's load and the
   renderer's read."
  [category]
  (cond
    (map? category)    (:category/id category)
    (vector? category) (second category)
    :else              nil))

(defsc-form LineItemForm [this props]
  {fo/id            line-item/id
   fo/attributes    [line-item/category line-item/item line-item/quantity
                     line-item/quoted-price line-item/subtotal]
   fo/field-styles  {:line-item/category :pick-one
                     :line-item/item     :pick-one}
   fo/field-options {:line-item/category
                     {po/query-key       :category/all-categories
                      po/query-component CategoryQuery
                      po/options-xform   (fn [_ options]
                                           (mapv (fn [{:category/keys [id label]}]
                                                   {:text (str label) :value [:category/id id]})
                                             (sort-by :category/label options)))
                      po/cache-time-ms   30000}
                     :line-item/item
                     {po/query-key        :item/all-items
                      po/query-component  ItemQuery
                      ;; Cache per selected category so switching categories re-queries. `category` may be
                      ;; a denormalized map (renderer) or a bare ident (trigger) — `category-id` handles both.
                      po/cache-key        (fn [_ {:line-item/keys [category]}]
                                            (keyword "item-list"
                                              (or (some-> (category-id category) str) "all")))
                      po/query-parameters (fn [_ _ {:line-item/keys [category]}]
                                            (when-let [cid (category-id category)]
                                              {:category/id cid}))
                      po/options-xform    (fn [_ options]
                                            (mapv (fn [{:item/keys [id name price]}]
                                                    {:text  (str name " - $" (math/numeric->str price))
                                                     :value [:item/id id]})
                                              (sort-by :item/name options)))
                      po/cache-time-ms    60000}}
   fo/layout        [[:line-item/category :line-item/item]
                     [:line-item/quantity :line-item/quoted-price :line-item/subtotal]]
   sfo/triggers     {:derive-fields
                     (fn [{:line-item/keys [quantity quoted-price] :as line-item}]
                       (assoc line-item
                         :line-item/subtotal (math/* (or quantity 0) (or quoted-price (math/zero)))))
                     :on-change
                     (fn [{:fulcro/keys [app]} {:fulcro/keys [state-map]} form-ident k _old new-value]
                       (case k
                         ;; Picking a category re-scopes the item picker and clears the stale item.
                         ;; `:on-change` ops are computed from the PRE-mutation state-map, so the new category
                         ;; isn't stored yet — inject `new-value` into the props we hand `load-options!` so the
                         ;; item picker loads/caches under the just-picked category (matching what the renderer,
                         ;; reading post-mutation props, will look up).
                         :line-item/category
                         (let [cls   (rc/registry-key->class ::LineItemForm)
                               props (assoc (get-in state-map form-ident) :line-item/category new-value)]
                           (po/load-options! app cls props line-item/item)
                           [(fops/apply-action update-in form-ident dissoc :line-item/item)])
                         ;; Picking an item copies its catalog price into the quoted price.
                         :line-item/item
                         (let [price (get-in state-map (conj (vec new-value) :item/price))]
                           [(fops/apply-action assoc-in (conj form-ident :line-item/quoted-price) price)])
                         []))}})
