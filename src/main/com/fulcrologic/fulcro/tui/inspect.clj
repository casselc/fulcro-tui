(ns com.fulcrologic.fulcro.tui.inspect
  "JVM (Clojure) Fulcro Inspect connector for TUI apps.

   This is the small CLJ shim that fills the only gap preventing a plain JVM Fulcro app from
   connecting to the standalone Fulcro Inspect (Electron) app: a `DevToolConnectionFactory`
   that speaks Sente over a real websocket from the JVM.

   The entire inspect event-generation pipeline (transactions, network, db-changed) and the
   devtools-remote transport (connection, target, resolvers) are already CLJC and ship transitively
   with Fulcro 3.10.x. The only CLJS-only piece is `com.fulcrologic.devtools.electron.target`
   (it uses `goog-define` and `enc/get-win-loc`). This namespace is a CLJ analog of that factory.

   Usage (debug only):

     (require '[com.fulcrologic.fulcro.tui.inspect :as tui-inspect])
     (tui-inspect/install!)                 ; register the JVM websocket factory ONCE
     (tui-inspect/add-inspect! app)         ; attach inspect to your TUI app

   Requires the Fulcro Inspect Electron app to be running (it hosts the websocket server on
   localhost:8237). Also requires the inspect pipeline to be enabled in CLJ via the JVM property:

     -Dcom.fulcrologic.fulcro.inspect=true

   (Without that property `fulcro.inspect.tool/add-fulcro-inspect!` is a no-op, because its body is
   wrapped in the `ilet` macro that only emits in CLJ when that property is \"true\".)"
  (:require
    [clojure.core.async :as async]
    [com.fulcrologic.devtools.common.built-in-mutations :as bi]
    [com.fulcrologic.devtools.common.connection :as cc]
    [com.fulcrologic.devtools.common.message-keys :as mk]
    [com.fulcrologic.devtools.common.protocols :as dp]
    [com.fulcrologic.devtools.common.target :as target]
    [com.fulcrologic.devtools.common.transit :as encode]
    [fulcro.inspect.tool :as it]
    [taoensso.encore :as enc]
    [taoensso.sente :as sente]
    [taoensso.timbre :as log])
  (:import (com.fulcrologic.devtools.common.connection Connection)))

;; Sente packer that matches what the Inspect server expects (transit, same as the electron target).
(deftype TransitPacker []
  taoensso.sente.interfaces/IPacker
  (pack [_ x] (encode/write x))
  (unpack [_ s] (encode/read s)))

(defn make-packer [] (->TransitPacker))

(def ^:dynamic *server-host* "localhost")
(def ^:dynamic *server-port* 8237)

(def backoff-ms #(enc/exp-backoff % {:max 1000}))

(defn start-ws-messaging!
  "Establish the Sente websocket client for `conn` and run the send/receive go-loops.
   This is a CLJ port of `com.fulcrologic.devtools.electron.target/start-ws-messaging!`:
   the only differences are (1) fixed host/port instead of `goog-define`, (2) no `get-win-loc`
   browser protocol sniffing, and (3) `:type :ws` (the JVM Sente client supports websockets only,
   not the ajax long-poll fallback)."
  [conn]
  (let [vconfig (.-vconfig ^Connection conn)
        {:keys [target-id sente-socket-client async-processor send-ch]} (cc/connection-config conn)]
    (when-not sente-socket-client
      (try
        (vswap! vconfig assoc :sente-socket-client
          (let [client (sente/make-channel-socket-client! "/chsk" "no-token-desired"
                         {:type           :ws
                          :protocol       :http
                          :host           *server-host*
                          :port           *server-port*
                          :packer         (make-packer)
                          :wrap-recv-evs? false
                          :backoff-ms-fn  backoff-ms})]
            (add-watch (:state client) ::open-watch
              (fn [_ _ {was-open? :open?} {:keys [open?]}]
                (when (not= was-open? open?)
                  ((:send-fn client) [:fulcrologic.devtool/event {mk/connected? open?
                                                                  mk/target-id  target-id}])
                  (async-processor [(bi/devtool-connected {:connected? open?
                                                           :target-id  target-id})]))))
            client))
        (catch Throwable e
          (log/error e "Failed to create JVM Sente client")))
      (log/info "Starting inspect websockets at:" *server-host* ":" *server-port*)
      ;; send loop
      (async/go-loop [attempt 1]
        (let [client (get @vconfig :sente-socket-client)]
          (if-not client
            (log/info "Shutting down inspect ws send loop.")
            (let [{:keys [state send-fn]} client
                  open? (:open? @state)]
              (if open?
                (when-let [data (async/<! send-ch)]
                  (send-fn [:fulcrologic.devtool/event data]))
                (async/<! (async/timeout (backoff-ms attempt))))
              (recur (if open? 1 (inc attempt)))))))
      ;; receive loop
      (async/go-loop [attempt 1]
        (let [client (get @vconfig :sente-socket-client)]
          (if-not client
            (log/info "Shutting down inspect ws recv loop.")
            (let [{:keys [state ch-recv]} client
                  open? (:open? @state)]
              (if open?
                (let [[event-type message] (:event (async/<! ch-recv))]
                  (when (= :fulcrologic.devtool/event event-type)
                    ;; handle-devtool-message is `defn-` in the published 0.2.8 jar (it is called
                    ;; cross-ns from the CLJS electron target — a minor upstream visibility nit).
                    ;; Invoke via the var so the POC works against the released artifact.
                    (#'cc/handle-devtool-message conn message)))
                (async/<! (async/timeout (backoff-ms attempt))))
              (recur (if open? 1 (inc attempt))))))))))

(deftype WebsocketClientConnectionFactory []
  dp/DevToolConnectionFactory
  (-connect! [_ {:keys [target-id] :as config}]
    (let [target-id (or target-id (random-uuid))
          vconfig   (volatile! (assoc config
                                 :send-ch (async/chan (async/dropping-buffer 10000))
                                 :active-requests {}
                                 :target-id target-id))
          conn      (cc/->Connection vconfig)]
      (start-ws-messaging! conn)
      conn)))

(defn install!
  "Register the JVM websocket DevToolConnectionFactory (call once)."
  []
  (target/set-factory! (->WebsocketClientConnectionFactory)))

(defn enable!
  "Force BOTH inspect enable-gates ON at runtime, so callers don't need JVM `-D` flags.

   There are TWO independent gates:
     * `com.fulcrologic.fulcro.inspect.inspect-client/INSPECT` (sysprop `com.fulcrologic.fulcro.inspect`)
       gates Fulcro's `ido`/`ilet` (including `fulcro.inspect.tool/add-fulcro-inspect!`).
     * `com.fulcrologic.devtools.common.target/INSPECT` (sysprop `com.fulcrologic.devtools.enabled`)
       gates `target/connect!` (the `ido` around the websocket connection itself).

   Both are read once at ns-load, so flipping the system property after load is not enough — we
   `alter-var-root` the realized vars directly."
  []
  (System/setProperty "com.fulcrologic.fulcro.inspect" "true")
  (System/setProperty "com.fulcrologic.devtools.enabled" "true")
  (require 'com.fulcrologic.fulcro.inspect.inspect-client
    'com.fulcrologic.devtools.common.target)
  (alter-var-root (requiring-resolve 'com.fulcrologic.fulcro.inspect.inspect-client/INSPECT)
    (constantly "true"))
  (alter-var-root (requiring-resolve 'com.fulcrologic.devtools.common.target/INSPECT)
    (constantly "true"))
  nil)

(defn add-inspect!
  "Enable inspect, install the JVM websocket factory, and attach Fulcro Inspect to `app`.

   This is a debug-only convenience: it force-enables both inspect gates (see `enable!`) so you do
   NOT need to start the JVM with any `-D` flags. Requires the standalone Fulcro Inspect (Electron)
   app to be running (websocket server on localhost:8237)."
  [app]
  (enable!)
  (install!)
  (it/add-fulcro-inspect! app))
