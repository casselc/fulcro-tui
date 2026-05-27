(ns com.fulcrologic.fulcro.tui.render-bench
  "JVM-only render benchmarks — Step 0 of the rendering-performance plan. NOT babashka.

   Run from a dedicated nREPL started with the `:bench` alias and guardrails OFF (so `>defn`s
   compile WITHOUT spec instrumentation and timings are accurate), e.g.:

     clojure -A:bench -Sdeps '{:deps {nrepl/nrepl {:mvn/version \"1.5.1\"}}}' -m nrepl.cmdline

   Then `(require 'com.fulcrologic.fulcro.tui.render-bench)` and call `(report)` (our pipeline,
   per-stage) and `(report-vs-display)` (head-to-head against JLine's Display).

   Everything here drives the EXISTING public functions; no production code is modified."
  (:require
   [com.fulcrologic.fulcro.tui.display-poc :as poc]
   [com.fulcrologic.fulcro.tui.elements :as e]
   [com.fulcrologic.fulcro.tui.engine :as engine]
   [criterium.core :as crit]))

;; ===========================================================================
;; Fixtures
;; ===========================================================================

(def sizes
  "Representative terminal sizes as [rows cols]: small, typical, large."
  [[24 80] [40 120] [50 200]])

(def ^:private filler "the quick brown fox jumps over the lazy dog ")

(defn- line-str
  "A pseudo-random-ish line of text exactly `cols` wide (deterministic in `i`)."
  [i cols]
  (let [s (str "Row " i ": " (apply str (take cols (drop (mod i 11) (cycle filler)))))]
    (subs s 0 (min cols (count s)))))

(defn content-tree
  "A full-screen `vbox` of `rows` styled text lines (~`cols` wide) with varied fg color + bold,
   to exercise SGR run-coalescing realistically."
  [rows cols]
  (apply e/vbox {:id "root"}
    (for [i (range rows)]
      (e/text {:id     (str "l" i)
               :color  (nth [nil :red :green :blue :yellow :cyan] (mod i 6))
               :bold   (zero? (mod i 3))
               :height 1}
        (line-str i cols)))))

(defn content-buffer
  "Paints `content-tree` into a `rows`x`cols` buffer (a full frame)."
  [rows cols]
  (engine/render-buffer
    (engine/place (content-tree rows cols) {:x 0 :y 0 :w cols :h rows})
    rows cols))

(defn- scroll-placed
  "A root viewport of height `rows` holding 2*`rows` lines, placed full-screen, for the scroll case."
  [rows cols]
  (engine/place
    (e/viewport {:id :vp :height rows}
      (apply e/vbox {:id "inner"}
        (for [i (range (* 2 rows))]
          (e/text {:id (str "s" i) :color (nth [nil :red :green] (mod i 3)) :height 1}
            (line-str i cols)))))
    {:x 0 :y 0 :w cols :h rows}))

(defn scroll-buffers
  "Returns `[at-top scrolled-one-row]` buffers for the scroll scenario — nearly every row differs."
  [rows cols]
  (let [p (scroll-placed rows cols)]
    [(engine/render-buffer (assoc p ::engine/scroll {:x 0 :y 0}) rows cols)
     (engine/render-buffer (assoc p ::engine/scroll {:x 0 :y 1}) rows cols)]))

(defn one-cell-change
  "Returns `buf` with a single center cell changed (the 1-keystroke scenario)."
  [buf]
  (let [{:keys [rows cols]} buf]
    (engine/put-cell buf (quot cols 2) (quot rows 2) \# {:fg :red :bold? true})))

(defn scenario-buffers
  "Returns `{:full :one-cell :scroll :noop}`, each a `[prev next]` pair (prev may be nil)."
  [rows cols]
  (let [b       (content-buffer rows cols)
        [s0 s1] (scroll-buffers rows cols)]
    {:full     [nil b]
     :one-cell [b (one-cell-change b)]
     :scroll   [s0 s1]
     :noop     [b b]}))

;; ===========================================================================
;; Timing helpers
;; ===========================================================================

(defn- mean-us
  "Mean execution time of 0-arg `f` in microseconds, via criterium quick-bench."
  [f]
  (* 1e6 (double (first (:mean (crit/quick-benchmark (f) {}))))))

(defn- alternating-us
  "Mean microseconds per call when alternating between two states, measured with a hand-rolled
   warmed nanoTime loop. `f` takes a boolean (which state to apply this call) and does the work;
   alternating guarantees every call is a real prev<->next transition (so stateful consumers like
   JLine Display do real diff work each call instead of a no-op)."
  ([f] (alternating-us f 2000 20000))
  ([f warmup iters]
   (let [flip (atom false)]
     (dotimes [_ warmup] (f (swap! flip not)))
     (let [t0 (System/nanoTime)]
       (dotimes [_ iters] (f (swap! flip not)))
       (/ (- (System/nanoTime) t0) (double iters) 1000.0)))))

;; ===========================================================================
;; Our pipeline — per-stage breakdown
;; ===========================================================================

(defn stage-results
  "Per-stage mean microseconds for one `rows`x`cols` size, plus op/byte counts."
  [rows cols]
  (let [tree    (content-tree rows cols)
        screen  {:x 0 :y 0 :w cols :h rows}
        placed  (engine/place tree screen)
        sc      (scenario-buffers rows cols)
        [_ full]   (:full sc)
        [b one]    (:one-cell sc)
        [s0 s1]    (:scroll sc)
        full-ops   (engine/diff nil full)
        one-ops    (engine/diff b one)
        scroll-ops (engine/diff s0 s1)]
    {:place              (mean-us #(engine/place tree screen))
     :render-buffer      (mean-us #(engine/render-buffer placed rows cols))
     :diff/full          (mean-us #(engine/diff nil full))
     :diff/one-cell      (mean-us #(engine/diff b one))
     :diff/scroll        (mean-us #(engine/diff s0 s1))
     :diff/noop          (mean-us #(engine/diff b b))
     :ops->ansi/full     (mean-us #(engine/ops->ansi full-ops))
     :ops->ansi/scroll   (mean-us #(engine/ops->ansi scroll-ops))
     :ops->ansi/one-cell (mean-us #(engine/ops->ansi one-ops))
     :frame/full         (mean-us #(engine/frame->ansi nil full {}))
     :frame/scroll       (mean-us #(engine/frame->ansi s0 s1 {}))
     :frame/one-cell     (mean-us #(engine/frame->ansi b one {}))
     :counts             {:full-ops        (count full-ops)
                          :scroll-ops      (count scroll-ops)
                          :full-ansi-len   (count (engine/ops->ansi full-ops))
                          :scroll-ansi-len (count (engine/ops->ansi scroll-ops))}}))

(defn report
  "Runs `stage-results` for every size in `sizes` and returns a map keyed by [rows cols]."
  []
  (into {} (for [[r c] sizes] [[r c] (stage-results r c)])))

;; ===========================================================================
;; Head-to-head: our output path vs JLine Display
;; ===========================================================================

(defn- ours-transition-us
  "Mean µs to compute the bytes for a prev<->next transition with our pipeline (diff + ops->ansi,
   the frame->ansi work), alternating directions so each call diffs a real change."
  [b0 b1]
  (alternating-us (fn [a?] (if a? (engine/frame->ansi b0 b1 {}) (engine/frame->ansi b1 b0 {})))))

(defn vs-display-results
  "For one size: µs/transition for our `frame->ansi` vs JLine `Display.update`, for the one-cell
   and scroll scenarios (the cases where prev/next differ)."
  [rows cols]
  (let [sc        (scenario-buffers rows cols)
        [b one]   (:one-cell sc)
        [s0 s1]   (:scroll sc)
        d-one     (poc/display-bench rows cols b one)
        d-scroll  (poc/display-bench rows cols s0 s1)]
    {:one-cell {:ours    (ours-transition-us b one)
                :display d-one}
     :scroll   {:ours    (ours-transition-us s0 s1)
                :display d-scroll}}))

(defn report-vs-display
  "Head-to-head table keyed by [rows cols]."
  []
  (into {} (for [[r c] sizes] [[r c] (vs-display-results r c)])))
