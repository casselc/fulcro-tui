(ns tui-demo.server.resolvers
  "Server-only Pathom resolvers for the demo's virtual report source attributes
   (`:account/all-accounts`, `:invoice/all-invoices`). These live here — not on the
   shared model attributes — so the model namespaces carry no datascript dependency
   and remain loadable on the (babashka) client."
  (:require
    [com.wsscode.pathom.connect :as pc]
    [tui-demo.server.queries :as queries]))

(pc/defresolver all-accounts-resolver [env _]
  {::pc/output [{:account/all-accounts [:account/id]}]}
  {:account/all-accounts (queries/get-all-accounts env)})

(pc/defresolver all-invoices-resolver [env _]
  {::pc/output [{:invoice/all-invoices [:invoice/id]}]}
  {:invoice/all-invoices (queries/get-all-invoices env)})

(def resolvers
  "Source-attribute resolvers for the demo reports."
  [all-accounts-resolver all-invoices-resolver])
