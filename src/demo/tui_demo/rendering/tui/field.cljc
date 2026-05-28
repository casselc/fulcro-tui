(ns tui-demo.rendering.tui.field
  "TUI field and control renderers for the RAD statechart engine.

   Field renderers have signature `(fn [env attribute])` (see `com.fulcrologic.rad.form/render-field`)
   and return a fulcro-tui node: an `hbox` of a label `text` plus an `input` whose `:on-change`
   informs the form statechart via `form/input-changed!`.

   Control renderers have signature `(fn {:keys [instance control-key control]})` (see
   `com.fulcrologic.rad.control/render-control`) and return a `button` or label+`input` node, wiring
   activation to the control's `:action`/`:onChange` and `control/set-parameter!`."
  (:require
    [clojure.string :as str]
    [com.fulcrologic.fulcro.algorithms.lambda :as lambda]
    [com.fulcrologic.fulcro.components :as comp]
    [com.fulcrologic.fulcro.mutations :as m]
    [com.fulcrologic.fulcro.raw.application :as rapp]
    [com.fulcrologic.fulcro.tui.elements :as e :refer [hbox text input button]]
    [com.fulcrologic.rad.attributes :as attr]
    [com.fulcrologic.rad.attributes-options :as ao]
    [com.fulcrologic.rad.control :as control]
    [com.fulcrologic.rad.form :as-alias rform]
    [com.fulcrologic.rad.form-render :as fr]
    [com.fulcrologic.rad.options-util :refer [?!]]
    [com.fulcrologic.rad.picker-options :as po]
    [com.fulcrologic.rad.statechart.form :as form]
    [com.fulcrologic.rad.type-support.decimal :as math]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Field renderers (form type->style->control)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- value->string
  "Returns a display string for an arbitrary field `value` (empty string for nil)."
  [value]
  (cond
    (nil? value) ""
    (string? value) value
    :else (str value)))

(defn- field-node-id
  "A focus/caret id for a field that is unique per form INSTANCE (so the same attribute on multiple
   subform rows — e.g. each line item's `:description` — gets a distinct id)."
  [form-instance qualified-key prefix]
  (keyword prefix (str (some-> (comp/get-ident form-instance) second)
                    "_" (namespace qualified-key) "_" (name qualified-key))))

(defn field-renderer
  "Returns a field render fn (`(fn [env attribute])`) that draws a labelled `input`. `string->model`
   converts the raw edited string into the value stored on the form (defaults to identity).

   On change we set the value **synchronously** with `m/set-value!!` (so the controlled terminal input
   reflects the keystroke on the same render — `input-changed!` alone posts an async statechart event
   and the input would snap back), and we ALSO fire `input-changed!` so the form's triggers
   (`:derive-fields`, validation) run and recompute read-only fields like subtotal/total."
  ([] (field-renderer identity))
  ([string->model]
   (fn [{::rform/keys [form-instance] :as env} {::attr/keys [qualified-key] :as attribute}]
     (let [{:keys [value visible? read-only? field-label invalid?
                   validation-message]} (form/field-context env attribute)]
       (when visible?
         (hbox {:height 1}
           (text {:width 16 :color (if invalid? :bright-red :cyan)}
             (str (or field-label (some-> qualified-key name str/capitalize))
               (when invalid? (str " (" validation-message ")"))))
           (input {:id        (field-node-id form-instance qualified-key "field")
                   :grow      1
                   :color     :bright-white
                   :value     (value->string value)
                   :on-change (fn [v & _]
                                (when-not read-only?
                                  (let [model (string->model v)]
                                    (m/set-value!! form-instance qualified-key model)
                                    (form/input-changed! env qualified-key model))))})))))))

(defn- parse-long-safe
  "Returns the integer value of string `s`, or nil if it does not parse."
  [s]
  (when (and (string? s) (re-matches #"-?\d+" (str/trim s)))
    #?(:clj (Long/parseLong (str/trim s)) :cljs (js/parseInt s 10))))

(defn- parse-decimal-safe
  "Returns the RAD decimal value of string `s`, or nil if it doesn't parse as a number."
  [s]
  (when (and (string? s) (seq (str/trim s)) (re-matches #"-?\d*\.?\d+" (str/trim s)))
    (try (math/numeric (str/trim s)) (catch #?(:clj Exception :cljs :default) _ nil))))

(def render-string-field
  "Renders a :string attribute as a labelled text input."
  (field-renderer identity))

(def render-int-field
  "Renders an :int attribute as a labelled text input, coercing the edited text to a Long when valid
   (a not-yet-numeric in-progress edit is kept as the raw string so typing still shows)."
  (field-renderer (fn [s] (or (parse-long-safe s) s))))

(def render-decimal-field
  "Renders a :decimal attribute as a labelled text input, coercing to a RAD decimal when valid so
   derived fields (subtotal/total) recompute; an in-progress edit is kept as the raw string."
  (field-renderer (fn [s] (or (parse-decimal-safe s) s))))

(def render-instant-field
  "Renders an :instant attribute as a labelled text input editing the raw date string."
  (field-renderer identity))

(defn render-ref-pick-one
  "Renders a to-one `:ref` field as a focusable `button` that cycles through the available options.
   Activating advances to the next option, setting the ref **synchronously** (so the change shows at
   once) and firing `form/input-changed!` so the form's triggers run.

   Options come from the field's `po/query-key` source loaded into app state (the demo preloads e.g.
   `:account/all-accounts` at startup — see `tui-demo.client.main`); each loaded entity is mapped to
   `{:text … :value [target-key id]}` via the field's `po/options-xform`. A TUI can't use React hooks
   (as the semantic-ui dropdown does), so this is a cycling button rather than a modal dropdown."
  [{::rform/keys [form-instance] :as env} {::attr/keys [qualified-key] :as attribute}]
  (let [{:keys [visible? read-only? field-label]} (form/field-context env attribute)
        props       (comp/props form-instance)
        state-map   (rapp/current-state (or comp/*app* (comp/any->app form-instance)))
        target-key  (ao/target attribute)
        field-opts  (merge attribute (form/get-field-options (comp/component-options form-instance) attribute))
        xform       (::po/options-xform field-opts)
        ;; Options are read from the target entity's normalized id-table (e.g. `:account/id`), which
        ;; the demo populates by preloading the picker source at startup. (A root query-key list is a
        ;; less stable source — routing/other loads can replace it — so we prefer the id table.)
        entities    (vec (vals (get state-map target-key)))
        options     (if xform
                      (xform attribute entities)
                      (mapv (fn [e] {:text (str (get e target-key)) :value [target-key (get e target-key)]}) entities))
        current     (get props qualified-key)
        current-val (when current [target-key (get current target-key)])
        current-lbl (some (fn [{:keys [text value]}] (when (= value current-val) text)) options)
        next-value  (fn []
                      (let [vs    (mapv :value options)
                            idx   (or (some (fn [[i v]] (when (= v current-val) i))
                                        (map-indexed vector vs)) -1)
                            nxt   (get vs (mod (inc idx) (max 1 (count vs))))]
                        nxt))]
    (when visible?
      (let [pick-id (field-node-id form-instance qualified-key "pick")]
       (hbox {:height 1}
        (text {:width 16 :color :cyan} (str (or field-label (name qualified-key))))
        (button {:id          pick-id
                 :color       :bright-magenta
                 :highlight   (e/focused? pick-id)
                 :on-activate (fn [] (when (and (not read-only?) (seq options))
                                       (let [nv (next-value)]      ;; an ident [target-key id]
                                         (m/set-value!! form-instance qualified-key nv)
                                         (form/input-changed! env qualified-key nv))))}
          (str " " (or current-lbl "(choose)") " ")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Controls (control type->style->control)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn render-button-control
  "Renders a `:button` control as a fulcro-tui `button`. Activation invokes the control's `:action`.
   The action is called arity-tolerantly: RAD control actions are usually `(fn [this] …)` (1-arg), but
   some take `[this control-key]`, so we adapt rather than always passing 2 args."
  [{:keys [instance control-key control]}]
  (let [{:keys [label action disabled? visible?]} control
        label    (?! label instance)
        visible? (or (nil? visible?) (?! visible? instance))]
    (when visible?
      (button {:id          (keyword "control" (name control-key))
               :color       :green
               :bold        true
               :highlight   (e/focused? (keyword "control" (name control-key)))
               :on-activate (fn [] (when (and action (not (?! disabled? instance)))
                                     ((lambda/->arity-tolerant action) instance control-key)))}
        (str " " (or label (name control-key)) " ")))))

(defn render-string-control
  "Renders a `:string` control as a labelled `input`. Edits call the control's `:onChange` (after
   storing the value via `control/set-parameter!`)."
  [{:keys [instance control-key control]}]
  (let [{:keys [label onChange placeholder visible?]} control
        label    (?! label instance)
        visible? (or (nil? visible?) (?! visible? instance))
        value    (control/current-value instance control-key)]
    (when visible?
      (hbox {:height 1}
        (text {:width 16 :color :cyan} (str (or label (name control-key))))
        (input {:id        (keyword "control" (name control-key))
                :grow      1
                :color     :bright-white
                :value     (value->string (or value placeholder))
                :on-change (fn [v & _]
                             (control/set-parameter! instance control-key v)
                             (when onChange (onChange instance v)))})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Field renderer registration (fulcro-rad-statecharts 0.1.5)
;;
;; The statechart form renders fields via the `fr/render-field` multimethod, dispatched on
;; `[attribute-type field-style]` (NOT the controls map). We register the demo's field types here;
;; requiring this ns installs them. `fr/render-field`'s `:default` just logs "No renderer installed".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod fr/render-field [:string :default]       [env attr] (render-string-field env attr))
(defmethod fr/render-field [:int :default]          [env attr] (render-int-field env attr))
(defmethod fr/render-field [:decimal :default]      [env attr] (render-decimal-field env attr))
(defmethod fr/render-field [:instant :default]      [env attr] (render-instant-field env attr))
(defmethod fr/render-field [:instant :date-at-noon] [env attr] (render-instant-field env attr))
(defmethod fr/render-field [:ref :pick-one]         [env attr] (render-ref-pick-one env attr))
