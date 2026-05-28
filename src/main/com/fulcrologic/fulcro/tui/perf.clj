(ns com.fulcrologic.fulcro.tui.perf
  "A tiny, babashka-compatible profiler — the core of Tufte (`p` profile points, a `profile`
   scope, and a println report) reimplemented with only plain atoms / volatiles / dynamic vars so
   it runs under SCI. (Real Tufte does NOT load under babashka: its `encore` dependency uses a
   `defrecord` implementing `clojure.lang.Counted`, which SCI rejects.)

   Design goals:

   * **Truly zero overhead unless explicitly built in.** `p` and `profile` are *compile-time*
     gated on the `fulcro.tui.perf` system property: unless it is set when the calling code is
     macro-expanded, both expand to just `(do body...)` — no runtime check, no volatile read, no
     trace of profiling in the compiled code. So these points are safe to leave in shipped render
     code, and the shipped library (compiled without the property) carries no profiling at all.
   * **No new dependencies.** Lives in `src/main` (the shipped render code calls `p` directly), but
     pulls in nothing.
   * **Self-time accounting.** Each `p` records both *total* (inclusive) and *self* (exclusive of
     nested `p`s) time, so it is safe to instrument recursive code (e.g. the paint walker) and read
     a meaningful per-id breakdown.

   Usage — first build the instrumentation IN by setting the property (JVM: `-Dfulcro.tui.perf=1`;
   babashka: `bb -Dfulcro.tui.perf=1 ...`), then while running the TUI:

     (perf/start!)        ; enable + reset
     ;; ... exercise the code / interact with the app ...
     (perf/report)        ; print the per-id table (self-time ranked)
     (perf/stop!)         ; disable

   Or scope it: `(perf/profile {} (render! app))` enables for the dynamic extent, prints a report,
   and returns the body value. Without the property set, `start!`/`stop!`/`report` still work but
   have nothing to measure, since no `p` points were compiled in."
  (:refer-clojure :exclude [memoize])
  (:require
   [clojure.string :as str]))

;; ===========================================================================
;; Compile-time gate
;; ===========================================================================

(defn- instrument?
  "Returns true when the `fulcro.tui.perf` system property is set. Called from the `p` and
   `profile` macros at MACRO-EXPANSION time: when it returns false they expand to just their body,
   so a build without the property compiles in no profiling whatsoever."
  []
  (some? (System/getProperty "fulcro.tui.perf")))

;; ===========================================================================
;; State
;; ===========================================================================

(def ^:private on?
  "Profiling enabled flag. A volatile so the `p` fast-path is a single field read."
  (volatile! false))

(defn enabled?
  "Returns true when profiling is currently capturing."
  []
  @on?)

(def ^:private stats
  "id -> {:n :total :self :min :max :samples}. `:total`/`:self`/`:min`/`:max` are nanoseconds;
   `:samples` is a capped vector of self-time samples used for percentiles."
  (atom {}))

(def ^:private wall
  "Wall-clock bracket of the current capture: {:t0 <nanoTime-or-nil> :ns <elapsed>}."
  (atom {:t0 nil :ns 0}))

(def ^:dynamic *children*
  "When inside a `p`, a `volatile!` accumulating the total (inclusive) ns of this point's DIRECT
   children, so the enclosing point can subtract it to get self time. `nil` at the top level."
  nil)

(def ^:private max-samples
  "Per-id cap on retained self-time samples (bounds memory + keeps `record!` cheap)."
  2048)

;; ===========================================================================
;; Recording
;; ===========================================================================

(defn record!
  "Folds one observation for `id` (`total-ns` inclusive, `self-ns` exclusive) into `stats`.
   Public because the `p` macro expands to a call here, but normally only called via `p`."
  [id total-ns self-ns]
  (swap! stats update id
    (fn [s]
      (let [{:keys [n total self mn mx samples]
             :or   {n 0 total 0 self 0 mn Long/MAX_VALUE mx 0 samples []}} s]
        {:n       (inc n)
         :total   (+ total (long total-ns))
         :self    (+ self (long self-ns))
         :mn      (min mn (long self-ns))
         :mx      (max mx (long self-ns))
         :samples (if (< (count samples) max-samples) (conj samples (long self-ns)) samples)})))
  nil)

(defmacro p
  "Profile point: times `body` under `id` (any value — keyword or syntax-quoted symbol) and returns
   its value.

   Gated at compile time on the `fulcro.tui.perf` system property (see `instrument?`): when the
   property is unset this expands to just `(do body...)` — zero overhead, nothing compiled in. When
   it is set, this compiles in instrumentation that is still gated at RUNTIME by `enabled?`, so
   captured timing only accrues between `start!` and `stop!`. Records total (inclusive) and self
   (exclusive of nested `p`s) time."
  [id & body]
  (if (instrument?)
    `(if (enabled?)
       (let [parent#  *children*
             kids#    (volatile! 0)
             t0#      (System/nanoTime)
             res#     (binding [*children* kids#] ~@body)
             elapsed# (- (System/nanoTime) t0#)]
         (record! ~id elapsed# (- elapsed# (long @kids#)))
         (when parent# (vswap! parent# + elapsed#))
         res#)
       (do ~@body))
    `(do ~@body)))

;; ===========================================================================
;; Control
;; ===========================================================================

(defn reset!
  "Clears all accumulated stats and restarts the wall-clock bracket."
  []
  (clojure.core/reset! stats {})
  (clojure.core/reset! wall {:t0 (System/nanoTime) :ns 0})
  nil)

(defn start!
  "Enables profiling and clears prior stats. Call before exercising the code to measure."
  []
  (reset!)
  (vreset! on? true)
  nil)

(defn stop!
  "Disables profiling and freezes the wall-clock elapsed."
  []
  (vreset! on? false)
  (swap! wall (fn [{:keys [t0] :as w}]
                (cond-> w t0 (assoc :ns (- (System/nanoTime) t0)))))
  nil)

(defn snapshot
  "Returns the current raw stats map (id -> aggregate), without printing."
  []
  @stats)

;; ===========================================================================
;; Reporting
;; ===========================================================================

(defn- pctl
  "Returns the `q` quantile (0.0..1.0) of the already-sorted `xs`, or 0 when empty."
  [sorted q]
  (if (seq sorted)
    (nth sorted (min (dec (count sorted)) (long (* q (count sorted)))))
    0))

(def ^:private id-col-width
  "Display width of the report's `id` column (matches the `%-34s` format below)."
  34)

(defn- abbreviate-id
  "Returns the profiling-point id string `s` shortened Clojure-stacktrace style so it fits the report's
   id column. Every dotted segment of the NAMESPACE except the last is collapsed to its first
   character; the last namespace segment and the name are kept in full. A leading `:` (keyword) is
   preserved; an id with no namespace is returned unchanged. As a final guard, an id still wider than
   the column is clipped from the FRONT (keeping the most-specific tail) behind a leading `…`.

   E.g. `:com.fulcrologic.fulcro.tui.engine/render-tree` -> `:c.f.f.t.engine/render-tree`."
  [s]
  (let [kw?    (str/starts-with? s ":")
        body   (cond-> s kw? (subs 1))
        slash  (str/index-of body "/")
        short  (if (and slash (str/index-of (subs body 0 slash) "."))
                 (let [nm   (subs body (inc slash))
                       segs (str/split (subs body 0 slash) #"\.")
                       abbr (conj (mapv #(subs % 0 1) (butlast segs)) (last segs))]
                   (str (str/join "." abbr) "/" nm))
                 body)
        out    (cond->> short kw? (str ":"))]
    (if (> (count out) id-col-width)
      (str "…" (subs out (- (count out) (dec id-col-width))))
      out)))

(defn- us
  "Formats nanoseconds `ns` as a microsecond string with one decimal."
  [ns]
  (format "%.1f" (/ (double ns) 1000.0)))

(defn- ms
  "Formats nanoseconds `ns` as a millisecond string with two decimals."
  [ns]
  (format "%.2f" (/ (double ns) 1.0e6)))

(defn report-string
  "Returns the formatted profiling report as a string (rows sorted by self time, descending).
   Percentages are share of total SELF time across all ids — i.e. where CPU time actually went."
  []
  (let [s        @stats
        sum-self (reduce + 0 (map :self (vals s)))
        rows     (->> s
                   (map (fn [[id {:keys [n total self mn mx samples]}]]
                          (let [srt (sort samples)]
                            {:id id :n n :total total :self self :mn mn :mx mx
                             :p50 (pctl srt 0.50) :p90 (pctl srt 0.90) :p99 (pctl srt 0.99)
                             :pct (if (pos? sum-self) (* 100.0 (/ (double self) sum-self)) 0.0)})))
                   (sort-by :self >))
        header   (format "%-34.34s %7s %8s %7s %9s %9s %9s %9s %9s %10s"
                   "id" "nCalls" "self%" "self(ms)" "mean(µs)" "p50(µs)" "p90(µs)" "p99(µs)"
                   "max(µs)" "total(ms)")
        line     (apply str (repeat (count header) \-))
        body     (for [{:keys [id n total self mn mx p50 p90 p99 pct]} rows]
                   (format "%-34.34s %7d %7.1f %8s %9s %9s %9s %9s %9s %10s"
                     (abbreviate-id (str id)) n pct (ms self)
                     (us (/ (double self) (max 1 n))) (us p50) (us p90) (us p99) (us mx) (ms total)))]
    (str/join "\n"
      (concat [header line] body
        [line
         (format "Σ self = %s ms across %d ids; wall = %s ms"
           (ms sum-self) (count rows) (ms (:ns @wall)))]))))

(defn report
  "Prints `report-string` to stdout and returns nil."
  []
  (println (report-string))
  nil)

(defmacro profile
  "Brackets `body` with `start!`/`stop!`, prints a `report`, and returns the body's value.
   `opts` is accepted for Tufte-API familiarity but currently ignored.

   Like `p`, gated at compile time on the `fulcro.tui.perf` system property: when it is unset this
   expands to just `(do body...)` (no start/stop/report), so it is a true no-op in a build without
   the property."
  [_opts & body]
  (if (instrument?)
    `(do
       (start!)
       (try ~@body
         (finally
           (stop!)
           (report))))
    `(do ~@body)))

;; ===========================================================================
;; Memoization / caching (encore-like, babashka-compatible)
;; ===========================================================================
;;
;; `taoensso.encore/cache` (its TTL memoizer) is NOT reachable from babashka: bb
;; bundles a 3-function `taoensso.encore` shim (`catching`, `ensure-vec`, `try*`) that
;; shadows the real jar even when you add it as a dependency. This is a small drop-in with
;; the same call shape — built on a plain atom of `{args {:v <delay> :udt <ms>}}` so it
;; loads and runs under SCI.

(defn cache
  "Returns a cached (memoized) version of referentially-transparent `f`.

   Called with just `f`, entries are cached forever. Called with an options map, supports
   time-based expiry. The options are:

     * `:ttl-ms` - Expire each entry this many milliseconds after it was computed. Omitted or
       0 means no expiry.
     * `:gc-every` - Sweep expired entries roughly once per this many calls (default 1000).
       Only relevant when `:ttl-ms` is set.

   Like `clojure.core/memoize` but de-raced: concurrent first-calls for the same args run `f`
   exactly once (the loser's `delay` is discarded unforced), so it is safe under contention.

   The returned fn also reads a command keyword as its FIRST argument:

     * `:cache/del`   - Drop the entry for the remaining args; returns nil. Pass
       `:cache/all` as the next arg — `(cached :cache/del :cache/all)` — to clear everything.
     * `:cache/fresh` - Force a recompute (and re-store) for the remaining args; returns the
       new value.

   Mirrors the call shape of `taoensso.encore/cache` (`:mem/del`/`:mem/fresh`/`:mem/all` are
   accepted as aliases) but is intentionally a small subset: no LRU/LFU size eviction."
  ([f] (cache {} f))
  ([{:keys [ttl-ms gc-every] :or {gc-every 1000}} f]
   (let [cache_   (atom {})
         calls    (atom 0)
         ttl      (long (or ttl-ms 0))
         ttl?     (pos? ttl)
         gc-every (long gc-every)
         stale?   (fn [udt now] (and ttl? (> (- (long now) (long udt)) ttl)))
         sweep!   (fn [now]
                    (swap! cache_
                      (fn [m]
                        (persistent!
                          (reduce-kv
                            (fn [acc k {:keys [udt]}]
                              (if (stale? udt now) (dissoc! acc k) acc))
                            (transient m) m)))))]
     (fn cached [& args]
       (let [a1 (first args)]
         (case a1
           (:cache/del :mem/del)
           (let [argn (next args)]
             (if (#{:cache/all :mem/all} (first argn))
               (clojure.core/reset! cache_ {})
               (swap! cache_ dissoc argn))
             nil)

           (let [fresh? (boolean (#{:cache/fresh :mem/fresh} a1))
                 kargs  (if fresh? (next args) args)
                 now    (System/currentTimeMillis)]
             (when (and ttl? (zero? (rem (swap! calls inc) gc-every)))
               (sweep! now))
             (-> (swap! cache_
                   (fn [m]
                     (let [e (get m kargs)]
                       (if (or fresh? (nil? e) (stale? (:udt e) now))
                         (assoc m kargs {:v (delay (apply f kargs)) :udt now})
                         m))))
               (get kargs) :v deref))))))))

(defn memoize
  "Encore-familiar wrapper over `cache`. `(memoize f)` caches forever; `(memoize ttl-ms f)`
   expires entries `ttl-ms` milliseconds after they are computed."
  ([f] (cache {} f))
  ([ttl-ms f] (cache {:ttl-ms ttl-ms} f)))
