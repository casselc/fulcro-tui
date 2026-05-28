(ns tui-demo.server.resolvers
  "Server-only Pathom resolvers for the demo's virtual report-source attributes and
   the derived `:line-item/category`. These live here — not on the shared model
   attributes — so the model namespaces carry no Datomic dependency and remain
   loadable on the (babashka) client."
  (:require
    [com.fulcrologic.rad.database-adapters.datomic-options :as do]
    [com.wsscode.pathom.connect :as pc]
    [datomic.client.api :as d]
    [tui-demo.server.queries :as queries]))

(pc/defresolver all-accounts-resolver [{:keys [query-params] :as env} _]
  {::pc/output [{:account/all-accounts [:account/id]}]}
  {:account/all-accounts (queries/get-all-accounts env query-params)})

(pc/defresolver all-invoices-resolver [{:keys [query-params] :as env} _]
  {::pc/output [{:invoice/all-invoices [:invoice/id]}]}
  {:invoice/all-invoices (queries/get-all-invoices env query-params)})

(pc/defresolver account-invoices-resolver [{:keys [query-params] :as env} _]
  {::pc/output [{:account/invoices [:invoice/id]}]}
  {:account/invoices (queries/get-customer-invoices env query-params)})

(pc/defresolver all-categories-resolver [{:keys [query-params] :as env} _]
  {::pc/output [{:category/all-categories [:category/id]}]}
  {:category/all-categories (queries/get-all-categories env query-params)})

(pc/defresolver all-items-resolver [{:keys [query-params] :as env} _]
  {::pc/output [{:item/all-items [:item/id]}]}
  {:item/all-items (queries/get-all-items env query-params)})

(pc/defresolver line-item-category-resolver [env {:line-item/keys [id]}]
  {::pc/input  #{:line-item/id}
   ::pc/output [{:line-item/category [:category/id]}]}
  (when-let [cid (queries/get-line-item-category env id)]
    {:line-item/category {:category/id cid}}))

(pc/defmutation set-account-active-mutation [env {:account/keys [id active?]}]
  {::pc/sym    'tui-demo.model.account/set-account-active
   ::pc/params [:account/id :account/active?]
   ::pc/output [:account/id :account/active?]}
  (let [conn (get-in env [do/connections :production])]
    (d/transact conn {:tx-data [{:account/id id :account/active? active?}]})
    {:account/id id :account/active? active?}))

(def resolvers
  "Source-attribute and derived resolvers (and mutations) for the demo."
  [all-accounts-resolver
   all-invoices-resolver
   account-invoices-resolver
   all-categories-resolver
   all-items-resolver
   line-item-category-resolver
   set-account-active-mutation])
