(ns com.fulcrologic.fulcro.tui.display-poc
  "Throwaway proof-of-concept: route our cell buffer through JLine's `org.jline.utils.Display`
   instead of our own `diff`/`ops->ansi`. JVM-ONLY (babashka bundles a different JLine and avoids
   JLine inner classes). Exists solely so Step 0 can measure Display-vs-ours head to head; it is
   NOT wired into the shipped render path.

   Strategy: convert a buffer's cells to one `AttributedString` per row (mapping our `:sgr` style
   to `AttributedStyle`), then call `Display.update`. A headless terminal (string streams,
   xterm-256color caps) lets `Display` run its real diff/cursor/clr_eol logic without a TTY."
  (:require
   [com.fulcrologic.fulcro.tui.engine :as engine])
  (:import
   (java.io ByteArrayInputStream ByteArrayOutputStream)
   (org.jline.terminal Terminal TerminalBuilder)
   (org.jline.utils AttributedString AttributedStringBuilder AttributedStyle Display)))

(defn- color-index
  "Maps one of our palette keywords to a JLine 0-15 color index (0-7 standard, 8-15 bright)."
  [kw]
  (let [c (engine/palette kw)]
    (if (>= c 90) (+ 8 (- c 90)) (- c 30))))

(defn- ->style
  "Returns the `AttributedStyle` for our `:sgr` style map."
  ^AttributedStyle [{:keys [fg bg bold? reverse?]}]
  (cond-> AttributedStyle/DEFAULT
    bold?    (.bold true)
    reverse? (.inverse true)
    fg       (.foreground (int (color-index fg)))
    bg       (.background (int (color-index bg)))))

(defn buffer->lines
  "Converts `buf` to a `java.util.List` of one `AttributedString` per row."
  [buf]
  (let [{:keys [rows cols cells]} buf]
    (mapv (fn [r]
            (let [sb (AttributedStringBuilder.)]
              (dotimes [c cols]
                (let [{:keys [ch sgr]} (nth cells (+ c (* r cols)))]
                  (.append sb (AttributedString. (str ch) (->style sgr)))))
              (.toAttributedString sb)))
      (range rows))))

(defn headless-terminal
  "Builds a non-system JLine terminal backed by in-memory streams with xterm-256color
   capabilities — enough for `Display` to emit real cursor/erase/scroll sequences."
  ^Terminal []
  (.. (TerminalBuilder/builder)
    (system false)
    (streams (ByteArrayInputStream. (byte-array 0)) (ByteArrayOutputStream.))
    (type "xterm-256color")
    (build)))

(defn display-bench
  "Mean microseconds per prev<->next transition using JLine `Display.update`, alternating
   directions so every call is a real diff. Builds the line lists once (conversion cost excluded);
   pass `:include-conversion? true` to fold the buffer->lines cost in."
  ([rows cols prev next] (display-bench rows cols prev next {}))
  ([rows cols prev next {:keys [include-conversion? warmup iters]
                         :or   {warmup 2000 iters 20000}}]
   (let [term  (headless-terminal)
         disp  (Display. term true)
         _     (.resize disp rows cols)
         l0    (buffer->lines prev)
         l1    (buffer->lines next)
         flip  (atom false)
         step  (if include-conversion?
                 (fn [a?] (.update disp (buffer->lines (if a? next prev)) 0))
                 (fn [a?] (.update disp (if a? l1 l0) 0)))]
     (try
       (dotimes [_ warmup] (step (swap! flip not)))
       (let [t0 (System/nanoTime)]
         (dotimes [_ iters] (step (swap! flip not)))
         (/ (- (System/nanoTime) t0) (double iters) 1000.0))
       (finally (.close term))))))
