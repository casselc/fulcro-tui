(ns tui-demo.model.model
  "Aggregates all RAD attributes for the demo. Pure CLJC and client/babashka-safe:
   no server/datascript dependency. Server-side resolvers live in
   `tui-demo.server.resolvers`; the Datascript id-resolvers are generated in the parser."
  (:require
    [com.fulcrologic.rad.attributes :as attr]
    [tui-demo.model.account :as account]
    [tui-demo.model.invoice :as invoice]
    [tui-demo.model.line-item :as line-item]))

(def all-attributes
  (vec (concat account/attributes invoice/attributes line-item/attributes)))

(def key->attribute (attr/attribute-map all-attributes))
