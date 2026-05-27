(ns com.fulcrologic.fulcro.tui.perf-spec
  "Tests for `perf/cache` (the babashka-compatible TTL memoizer) and `perf/memoize`.

   Like the rest of the suite this is fulcro-spec, and it runs under BOTH the JVM
   (`clojure -M:test:test-runner`) and babashka (`bb test`) — fulcro-spec >= 3.2.10 loads
   under bb, and the code under test is dependency-free."
  (:require
   [com.fulcrologic.fulcro.tui.perf :as perf]
   [fulcro-spec.core :refer [=> assertions component specification]]))

(defn counting-fn
  "Returns `[counter f]` where `f` increments `counter` on every (non-cached) call and returns
   `[arg call-number]`."
  []
  (let [calls (atom 0)]
    [calls (fn [x] (swap! calls inc) [x @calls])]))

(specification {:covers {`perf/cache "0ef621"}} "cache"
  (component "memoization by args"
    (let [[calls f] (counting-fn)
          g         (perf/cache f)]
      (assertions
        "repeated calls with the same arg compute f once"
        (= (g 1) (g 1) (g 1)) => true
        @calls => 1
        "distinct args are cached independently"
        (= (g 1) (g 2)) => false
        @calls => 2)))

  (component "no ttl never expires"
    (let [[calls f] (counting-fn)
          g         (perf/cache f)]
      (g 1)
      (Thread/sleep 30)
      (g 1)
      (assertions
        "entry is kept regardless of elapsed time"
        @calls => 1)))

  (component "ttl expires entries"
    (let [[calls f] (counting-fn)
          g         (perf/cache {:ttl-ms 30} f)]
      (assertions
        "within ttl the value is cached"
        (g 1) => [1 1]
        (g 1) => [1 1]
        @calls => 1)
      (Thread/sleep 60)
      (assertions
        "after ttl the value is recomputed"
        (g 1) => [1 2]
        @calls => 2)))

  (component ":cache/fresh forces a recompute"
    (let [[calls f] (counting-fn)
          g         (perf/cache f)]
      (g 7)
      (assertions
        ":cache/fresh recomputes, re-stores, and returns the new value"
        (g :cache/fresh 7) => [7 2]
        @calls => 2
        "a subsequent plain call sees the refreshed value (no recompute)"
        (g 7) => [7 2]
        @calls => 2
        ":mem/fresh is an accepted alias"
        (do (g :mem/fresh 7) @calls) => 3)))

  (component ":cache/del drops entries"
    (let [[calls f] (counting-fn)
          g         (perf/cache f)]
      (g 1) (g 2)
      (assertions
        ":cache/del returns nil and drops a single entry; next call recomputes it"
        (g :cache/del 1) => nil
        (do (g 1) @calls) => 3
        "the un-deleted entry is untouched"
        (do (g 2) @calls) => 3
        ":cache/del :cache/all clears everything"
        (g :cache/del :cache/all) => nil
        (do (g 1) (g 2) @calls) => 5)))

  (component "zero-arg fn"
    (let [calls (atom 0)
          g     (perf/cache (fn [] (swap! calls inc)))]
      (assertions
        "a no-arg cached fn computes once"
        (g) => 1
        (g) => 1
        @calls => 1)))

  (component "is de-raced"
    (let [calls (atom 0)
          g     (perf/cache (fn [x] (swap! calls inc) (Thread/sleep 50) [x @calls]))
          rs    (mapv deref (doall (repeatedly 16 #(future (g 1)))))]
      (assertions
        "concurrent first-calls for the same args run f exactly once"
        @calls => 1
        "every concurrent caller observes the same value"
        (apply = rs) => true))))

(specification {:covers {`perf/memoize "ea8871"}} "memoize"
  (component "(memoize f) caches forever"
    (let [[calls f] (counting-fn)
          g         (perf/memoize f)]
      (g 1) (g 1)
      (assertions
        "f is called once"
        @calls => 1)))

  (component "(memoize ttl-ms f) expires"
    (let [[calls f] (counting-fn)
          g         (perf/memoize 30 f)]
      (g 1)
      (Thread/sleep 60)
      (g 1)
      (assertions
        "f is recomputed after the ttl"
        @calls => 2))))
