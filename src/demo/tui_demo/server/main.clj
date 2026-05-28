(ns tui-demo.server.main
  "Entry point for the demo server: seeds an in-memory Datomic Local DB, installs it
   on the parser, and starts the http-kit `/api` server."
  (:require
    [tui-demo.server.http :as http]
    [tui-demo.server.parser :as parser]
    [tui-demo.server.seed :as seed])
  (:gen-class))

(defn -main
  "Seeds the database, wires it into the parser, and starts the HTTP server."
  [& _args]
  (parser/set-connection! (seed/fresh-conn))
  (http/start!)
  (println "listening on" http/port))
