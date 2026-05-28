(ns tui-demo.ui.item-forms
  "RAD (statechart) form + report for catalog Items. `ItemForm` edits a single item
   (category pick-one, name, description, in-stock, price); `InventoryReport` lists
   items with a server-backed category picker filter, client-side category filtering,
   and sortable name/category columns."
  (:require
    [com.fulcrologic.fulcro.raw.components :as rc]
    [com.fulcrologic.rad.control :as control]
    [com.fulcrologic.rad.form-options :as fo]
    [com.fulcrologic.rad.picker-options :as po]
    [com.fulcrologic.rad.report-options :as ro]
    [com.fulcrologic.rad.statechart.form :as form :refer [defsc-form]]
    [com.fulcrologic.rad.statechart.form-options :as sfo]
    [com.fulcrologic.rad.statechart.report :as report :refer [defsc-report]]
    [com.fulcrologic.rad.type-support.decimal :as math]
    [com.fulcrologic.statecharts.integration.fulcro.routing :as scr]
    [tui-demo.model.category :as category]
    [tui-demo.model.item :as item]))

(def report-route
  "Registry keyword of the inventory report (route target after save/cancel)."
  :tui-demo.ui.item-forms/InventoryReport)

(def CategoryQuery
  "Normalizing query component for category picker options."
  (rc/nc [:category/id :category/label] {:componentName ::CategoryQuery}))

(defsc-form ItemForm [this props]
  {fo/id            item/id
   fo/title         "Edit Item"
   fo/cancel-route  report-route
   fo/attributes    [item/name item/category item/description item/in-stock item/price]
   fo/field-styles  {:item/category :pick-one
                     :item/description :multi-line}
   fo/field-options {:item/category
                     {po/query-key       :category/all-categories
                      po/query-component CategoryQuery
                      po/options-xform   (fn [_ options]
                                           (mapv (fn [{:category/keys [id label]}]
                                                   {:text (str label) :value [:category/id id]})
                                             (sort-by :category/label options)))
                      po/cache-time-ms   30000}}
   fo/layout        [[:item/name :item/category]
                     [:item/description]
                     [:item/in-stock :item/price]]
   ;; After a successful save, return to the report (on-saved doesn't navigate on its own).
   sfo/triggers     {:saved
                     (fn [{:fulcro/keys [app]} _data _form-ident]
                       (scr/route-to! app report-route {})
                       nil)}})

(defsc-report InventoryReport [this props]
  {ro/title               "Inventory"
   ro/source-attribute    :item/all-items
   ro/row-pk              item/id
   ro/columns             [item/name category/label item/price item/in-stock]
   ro/row-query-inclusion [{:item/category [:category/id :category/label]}]
   ro/column-formatters   {:category/label (fn [_ _ row _] (get-in row [:item/category :category/label]))
                           :item/price     (fn [_ v _ _] (str "$" (math/numeric->str (or v (math/zero)))))}
   ;; Client-side filter: keep a row only when its category matches the picked label ("" = all).
   ro/row-visible?        (fn [{::keys [category]} row]
                            (let [row-cat (get-in row [:item/category :category/label])]
                              (or (nil? category) (= "" category) (= category row-cat))))
   ro/initial-sort-params {:sort-by          :item/name
                           :ascending?       true
                           :sortable-columns #{:item/name :category/label :item/price :item/in-stock}}
   ;; The category label is nested under :item/category, so the default comparator (which reads the
   ;; sort-by key directly off the row) would sort on the raw ident. Resolve the nested label here.
   ro/compare-rows        (fn [{:keys [sort-by ascending?]} a b]
                            (let [row-val (fn [row] (if (= sort-by :category/label)
                                                      (get-in row [:item/category :category/label])
                                                      (get row sort-by)))
                                  c       (try (compare (row-val a) (row-val b)) (catch Throwable _ 0))]
                              (if ascending? c (- c))))
   ro/form-links          {item/name ItemForm}
   ro/controls            {::category {:type             :picker
                                       :local?           true
                                       :label            "Category"
                                       :default-value    ""
                                       :action           (fn [this] (report/filter-rows! this))
                                       po/cache-time-ms   30000
                                       ;; Distinct cache-key: this filter stores label STRING values
                                       ;; ("All"/"Hardware"), whereas the ItemForm category FIELD stores
                                       ;; ident values under the default :category/all-categories key.
                                       ;; Sharing a key would clobber the field's options.
                                       po/cache-key       :inventory.filter/category
                                       po/query-key       :category/all-categories
                                       po/query-component CategoryQuery
                                       po/options-xform   (fn [_ categories]
                                                            (into [{:text "All" :value ""}]
                                                              (map (fn [{:category/keys [label]}]
                                                                     {:text label :value label}))
                                                              (sort-by :category/label categories)))}
                           ::new      {:type     :button
                                       :local?   true
                                       :label    "New Item"
                                       :action   (fn [this] (form/create! this ItemForm))}}
   ro/control-layout      {:action-buttons [::new]
                           :inputs         [[::category]]}
   ro/run-on-mount?       true
   ro/route               "inventory"})
