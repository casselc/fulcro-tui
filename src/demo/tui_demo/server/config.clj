(ns tui-demo.server.config
  "Datomic Local configuration for the demo server. Uses an in-memory (`:mem`)
   `:dev-local` database — nothing is written to disk, and a fresh process always
   starts empty (then gets seeded). JVM-only (Datomic Local does not run under
   babashka)."
  (:require
    [com.fulcrologic.rad.database-adapters.datomic-options :as do]))

(def datomic-config
  "Config map for `datomic-cloud/start-databases`: a single in-memory `:production`
   database keyed `:main`."
  {do/databases {:main {:datomic/schema   :production
                        :datomic/client   {:server-type :dev-local
                                           :storage-dir :mem
                                           :system      "tui-demo"}
                        :datomic/database "demo"}}})
