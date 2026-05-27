(ns tui-demo.server.http
  "http-kit server exposing a standard Fulcro transit `/api` endpoint that
   delegates EQL to the demo Pathom parser."
  (:require
    [com.fulcrologic.fulcro.server.api-middleware :as server]
    [org.httpkit.server :as http]
    [taoensso.timbre :as log]
    [tui-demo.server.parser :as parser]))

(defonce ^{:doc "Holds the running http-kit stop function, if any."} server (atom nil))

(def ^{:doc "TCP port the demo server listens on."} port 3001)

(defn- wrap-api
  "Routes requests for `uri` to the parser; everything else hits `handler`."
  [handler uri]
  (fn [request]
    (if (= uri (:uri request))
      (server/handle-api-request (:transit-params request)
        (fn [query] (parser/parser {:ring/request request} query)))
      (handler request))))

(def ^{:doc "The ring handler: transit `/api` over a 404 fallthrough."}
  handler
  (-> (fn [_req] {:status 404 :body ""})
    (wrap-api "/api")
    (server/wrap-transit-params {})
    (server/wrap-transit-response {})))

(defn start!
  "Starts the http-kit server on `port`. Idempotent."
  []
  (when-not @server
    (reset! server (http/run-server handler {:port port}))
    (log/info "HTTP server listening on" port))
  @server)

(defn stop!
  "Stops the running http-kit server, if any."
  []
  (when-let [stop-fn @server]
    (stop-fn)
    (reset! server nil)
    (log/info "HTTP server stopped.")))
