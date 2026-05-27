(ns tui-demo.client.remote
  "A synchronous Fulcro HTTP remote for the terminal client. Speaks the standard
   Fulcro **transit** wire format to the server's `/api`, so Fulcro's extended types
   (tempids, etc.) round-trip correctly on form saves.

   Runs on the JVM and under babashka: it uses `babashka.http-client` (bundled with bb)
   for the blocking POST and `com.fulcrologic.fulcro.headless.loopback-remotes/sync-remote`
   to adapt the EQL handler into a Fulcro remote (which yields `{:status-code 200 :body …}`
   to the tx-processing layer). Being synchronous suits the TUI's single-threaded loop."
  (:require
    [babashka.http-client :as http]
    [clojure.walk :as walk]
    [com.fulcrologic.fulcro.algorithms.transit :as transit]
    [com.fulcrologic.fulcro.headless.loopback-remotes :as lb]))

(defn- drop-fns
  "Replace any function values anywhere in `eql` with nil. Request data must be plain
   EDN; a stray fn (e.g. a closure left in a control parameter) would otherwise blow up
   transit encoding. Defensive — protects against careless params."
  [eql]
  (walk/postwalk (fn [x] (if (fn? x) nil x)) eql))

(defn encode-request
  "Transit-encode request `eql` for the wire. Metadata is ALWAYS disabled
   (`:metadata? false`) — Fulcro queries carry `^{:component Class}` metadata, and the
   class is a fn, which is not transit-serializable. Functions are also scrubbed from
   the data itself via `drop-fns`."
  [eql]
  (transit/transit-clj->str (drop-fns eql) {:metadata? false}))

(defn transit-remote
  "Returns a Fulcro remote that POSTs transit-encoded `eql` to `<base-url>/api` and
   decodes the transit response body. `base-url` e.g. \"http://localhost:3001\"."
  [base-url]
  (let [api-url (str base-url "/api")]
    (lb/sync-remote
      (fn [eql]
        (let [resp (http/post api-url
                     {:headers {"Content-Type" "application/transit+json"
                                "Accept"       "application/transit+json"}
                      :body    (encode-request eql)
                      :throw   false})]
          (if (= 200 (:status resp))
            (transit/transit-str->clj (:body resp))
            (throw (ex-info "Demo API request failed"
                     {:status (:status resp) :body (:body resp) :eql eql}))))))))
