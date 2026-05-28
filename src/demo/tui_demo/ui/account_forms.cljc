(ns tui-demo.ui.account-forms
  "RAD (statechart) forms + report for Accounts. `AccountForm` edits an account with a
   role enum, an active? toggle, email validation, a primary-address component subform,
   and a to-many additional-addresses subform. `BriefAccountForm` is the lightweight
   create form used as the invoice customer quick-create. `AccountList` lists accounts
   with a name filter, a show-inactive? toggle (server re-query), sortable columns, and
   Enable/Disable row actions."
  (:require
    [clojure.string :as str]
    [com.fulcrologic.fulcro.components :as comp]
    [com.fulcrologic.rad.form-options :as fo]
    [com.fulcrologic.rad.report-options :as ro]
    [com.fulcrologic.rad.statechart.form :as form :refer [defsc-form]]
    [com.fulcrologic.rad.statechart.report :as report :refer [defsc-report]]
    [tui-demo.model.account :as account]
    [tui-demo.ui.address-forms :refer [AddressForm]]))

(defsc-form AccountForm [this props]
  {fo/id             account/id
   fo/title          "Edit Account"
   fo/cancel-route   :tui-demo.ui.account-forms/AccountList
   fo/attributes     [account/name account/role account/email account/active?
                      account/primary-address account/addresses]
   fo/default-values {:account/active?         true
                      :account/primary-address {}
                      :account/addresses       [{}]}
   fo/layout         [[:account/name :account/role]
                      [:account/email :account/active?]]
   fo/subforms       {:account/primary-address {fo/ui                      AddressForm
                                                fo/title                   "Primary Address"
                                                ::form/autocreate-on-load? true}
                      :account/addresses       {fo/ui          AddressForm
                                                fo/title       "Additional Addresses"
                                                fo/can-add?    (fn [_ _] true)
                                                fo/can-delete? (fn [parent _]
                                                                 (< 1 (count (:account/addresses (comp/props parent)))))}}})

(defsc-form BriefAccountForm [this props]
  {fo/id             account/id
   fo/title          "New Customer"
   fo/attributes     [account/name account/role account/email account/active?]
   fo/layout         [[:account/name :account/role]
                      [:account/email :account/active?]]
   fo/default-values {:account/active? true}})

(defsc-report AccountList [this props]
  {ro/title               "Accounts"
   ro/source-attribute    :account/all-accounts
   ro/row-pk              account/id
   ro/columns             [account/name account/active?]
   ro/column-formatters   {:account/active? (fn [_ v _ _] (if v "Yes" "No"))}
   ro/column-headings     {:account/name "Account Name"}
   ro/form-links          {account/name AccountForm}
   ro/initial-sort-params {:sort-by          :account/name
                           :ascending?       true
                           :sortable-columns #{:account/name :account/active?}}
   ;; Client-side name filter (the show-inactive? toggle re-queries the server instead).
   ro/row-visible?        (fn [{::keys [filter-name]} {:account/keys [name]}]
                            (let [nm     (some-> name str/lower-case)
                                  target (some-> filter-name str/trim str/lower-case)]
                              (or (nil? target) (= "" target) (and nm (str/includes? nm target)))))
   ro/controls            {::new           {:type   :button
                                            :local? true
                                            :label  "New Account"
                                            :action (fn [this] (form/create! this AccountForm))}
                           ::filter-name   {:type        :string
                                            :local?      true
                                            :placeholder "name filter"
                                            :label       "Filter"
                                            :onChange    (fn [this _] (report/filter-rows! this))}
                           :show-inactive? {:type          :boolean
                                            :local?        false
                                            :default-value false
                                            :label         "Show inactive?"
                                            :onChange      (fn [this _] (report/run-report! this))}}
   ro/control-layout      {:action-buttons [::new]
                           :inputs         [[::filter-name] [:show-inactive?]]}
   ro/row-actions         [{:label     "Enable"
                            :action    (fn [report-instance {:account/keys [id]}]
                                         (comp/transact! report-instance
                                           [(account/set-account-active {:account/id id :account/active? true})])
                                         (report/run-report! report-instance))
                            :disabled? (fn [_ row] (:account/active? row))}
                           {:label     "Disable"
                            :action    (fn [report-instance {:account/keys [id]}]
                                         (comp/transact! report-instance
                                           [(account/set-account-active {:account/id id :account/active? false})])
                                         (report/run-report! report-instance))
                            :disabled? (fn [_ row] (not (:account/active? row)))}]
   ro/run-on-mount?       true
   ro/route               "accounts"})
