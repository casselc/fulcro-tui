(ns com.fulcrologic.fulcro.tui.terminal
  "A self-contained terminal abstraction for a TUI rendering target.

   This namespace OWNS the normalized key-event model that downstream TUI code consumes, a pure
   (JLine-free) key decoder, a `Terminal` protocol, a real JLine-backed implementation, and an
   atom-backed fake (`string-terminal`) used for tests.

   This is JVM/babashka only (plain `.clj`)."
  (:require
   [com.fulcrologic.guardrails.core :refer [>def >defn >defn- => ?]]
   [clojure.spec.alpha :as s])
  (:import
   (org.jline.terminal TerminalBuilder)
   (org.jline.utils NonBlockingReader)))

;; =============================================================================
;; Normalized key-event model
;; =============================================================================

(def special-keys
  "The set of keyword values allowed as a special-key `:key`."
  #{:enter :tab :backtab :escape :backspace :delete
    :up :down :left :right
    :home :end :page-up :page-down})

(defn single-codepoint-string?
  "Returns true if `s` is a string consisting of exactly one unicode code point. Note that an
   astral/supplementary code point occupies two UTF-16 chars yet is still a single code point, so
   this counts code points rather than `count` (which counts UTF-16 code units)."
  [s]
  (and (string? s)
       (pos? (count s))
       (= 1 (.codePointCount ^String s 0 (count s)))))

(>def ::key (s/or :special special-keys
                  :printable single-codepoint-string?))
(>def ::char (s/nilable string?))
(>def ::ctrl? boolean?)
(>def ::alt? boolean?)
(>def ::shift? boolean?)
(>def ::raw (s/or :code-point int? :code-points (s/coll-of int? :kind vector?)))

(>def ::key-event
      (s/keys :req-un [::key ::ctrl? ::alt? ::shift?]
              :opt-un [::char ::raw]))

(>defn key-event
       "Returns a normalized key event map. `key` is a 1-char string (printable) or a keyword from
   `special-keys`. The remaining values default to a non-modified, non-char event; pass `opts`
   (a map of any of `:char :ctrl? :alt? :shift? :raw`) to override."
       ([key]
        [::key => ::key-event]
        (key-event key {}))
       ([key opts]
        [::key map? => ::key-event]
        (merge {:key   key
                :char  nil
                :ctrl? false
                :alt?  false
                :shift? false}
               opts)))

;; =============================================================================
;; Pure key decoder (testable WITHOUT JLine)
;; =============================================================================

(>defn- ctrl-letter
        "Returns the lowercase letter string for a control code `n` in 1..26 (1 -> \"a\" .. 26 -> \"z\")."
        [n]
        [int? => string?]
        (str (char (+ (int \a) (dec n)))))

(>defn- printable-event
        "Returns a printable `::key-event` for the unicode code point `cp` (the `:key` and `:char` are the
   1-char string for `cp`)."
        [cp]
        [int? => ::key-event]
        (let [s (String. (Character/toChars cp))]
          (key-event s {:char s :raw cp})))

(>defn- csi-event
        "Returns `[event remaining]` for a CSI (ESC `[`) sequence, given `rest-ints` (the ints AFTER the
   leading `27 91`). Returns nil if the sequence is not recognized so the caller can fall back."
        [rest-ints]
        [(s/coll-of int?) => (? (s/tuple ::key-event (s/coll-of int?)))]
        (let [v   (vec rest-ints)
              f   (first v)
              raw-2 (fn [k]            ; two-byte CSI like 27 91 65 ("A")
                      [(key-event k {:raw [27 91 f]}) (subvec v 1)])
              raw-3 (fn [k]            ; three-byte CSI like 27 91 51 126 ("3~")
                      [(key-event k {:raw [27 91 f 126]}) (subvec v 2)])]
          (cond
            (= f 65) (raw-2 :up)
            (= f 66) (raw-2 :down)
            (= f 67) (raw-2 :right)
            (= f 68) (raw-2 :left)
            (= f 72) (raw-2 :home)
            (= f 70) (raw-2 :end)
            (= f 90) (raw-2 :backtab)                              ; ESC [ Z = Shift-Tab
      ;; numeric forms terminated by ~ (126)
            (and (= (second v) 126))
            (case (int f)
              51 (raw-3 :delete)                                  ; 3~
              49 (raw-3 :home)                                    ; 1~
              52 (raw-3 :end)                                     ; 4~
              53 (raw-3 :page-up)                                 ; 5~
              54 (raw-3 :page-down)                               ; 6~
              nil)
            :else nil)))

(>defn decode-key
       "Decodes the next key from a sequence of input code points `ints`.

   Returns `[event remaining-ints]`, consuming exactly the code points for one key, or `nil` when
   `ints` is empty. The decoder is pure and contains no JLine/IO dependency. Handles:

   * printable ASCII / multi-byte unicode code points -> printable event
   * 9 -> `:tab`; 10 or 13 -> `:enter`; 8 or 127 -> `:backspace`
   * 27 alone (nothing following) -> `:escape`
   * CSI cursor/edit sequences (`27 91 ...`) -> arrows, home/end, delete, page-up/down
   * control combos 1..26 -> `{:ctrl? true :key \"a\"..\"z\"}` (tab/enter handled first)"
       [ints]
       [(s/coll-of int?) => (? (s/tuple ::key-event (s/coll-of int?)))]
       (let [v (vec ints)]
         (when (seq v)
           (let [c    (int (first v))
                 rest (subvec v 1)]
             (cond
               (= c 9) [(key-event :tab {:raw c}) rest]
               (or (= c 10) (= c 13)) [(key-event :enter {:raw c}) rest]
               (or (= c 8) (= c 127)) [(key-event :backspace {:raw c}) rest]
               (= c 27)
               (cond
                 (empty? rest) [(key-event :escape {:raw c}) rest]
                 (= (int (first rest)) 91)
                 (or (csi-event (subvec v 2))
              ;; unrecognized CSI: treat ESC as escape, leave the rest
                     [(key-event :escape {:raw c}) rest])
            ;; ESC + something else: treat ESC as escape (alt-combos not modeled here)
                 :else [(key-event :escape {:raw c}) rest])
          ;; control combos 1..26 (9/13 already handled above)
               (and (>= c 1) (<= c 26))
               [(key-event (ctrl-letter c) {:ctrl? true :raw c}) rest]
          ;; printable / multi-byte unicode
               (>= c 32) [(printable-event c) rest]
          ;; anything else (e.g. 0): consume as printable code point best-effort
               :else [(printable-event c) rest])))))

;; =============================================================================
;; Terminal protocol
;; =============================================================================

(defprotocol Terminal
  "Abstraction over a terminal device. Coordinates are 0-based (`x` column, `y` row)."
  (t-size [t] "Returns `{:rows R :cols C}`.")
  (t-read-key [t] "Returns the next normalized key event (blocking for real terminals), or nil on EOF/empty.")
  (t-write! [t s] "Writes string `s` to the terminal (no flush).")
  (t-flush! [t] "Flushes buffered output.")
  (t-set-cursor! [t x y visible?] "Positions the hardware cursor at 0-based (`x`,`y`) and shows/hides it.")
  (t-enter! [t] "Enters raw mode + alternate screen + hides the cursor.")
  (t-leave! [t] "Restores: shows cursor, leaves alt screen, exits raw mode, closes.")
  (t-sync-supported? [t] "Returns true if synchronized output (DEC 2026 / terminfo Sync) is available.")
  (t-on-resize! [t handler]
    "Registers zero-arg `handler` to be invoked when the terminal's size changes (e.g. SIGWINCH on a
     real terminal). At most one handler is kept; registering again replaces it. `nil` clears it."))

;; ANSI control strings
(def ^:private ansi-alt-screen-enter "[?1049h")
(def ^:private ansi-alt-screen-leave "[?1049l")
(def ^:private ansi-cursor-hide "[?25l")
(def ^:private ansi-cursor-show "[?25h")

(>defn cursor-position-string
       "Returns the ANSI escape sequence that moves the cursor to 0-based (`x`,`y`). ANSI is 1-based, so
   both are incremented."
       [x y]
       [int? int? => string?]
       (str "[" (inc y) ";" (inc x) "H"))

;; =============================================================================
;; JLine implementation
;; =============================================================================

(defn- read-key-from-reader
  "Reads the next normalized key from a JLine `NonBlockingReader`. Reads one code point (blocking);
   if it is ESC, peeks (with a short timeout) for a following CSI sequence to disambiguate a bare
   ESC from arrow/edit keys, accumulating available ints and running them through `decode-key`.
   Returns nil on EOF."
  [^NonBlockingReader reader]
  (let [c (.read reader)]
    (cond
      (= c NonBlockingReader/EOF) nil
      (= c 27)
      ;; gather any immediately-available following bytes for CSI disambiguation
      (let [acc (transient [27])]
        (loop []
          (let [n (.read reader 5)]                         ; 5ms peek window
            (when (and (not= n NonBlockingReader/READ_EXPIRED)
                       (not= n NonBlockingReader/EOF))
              (conj! acc (int n))
              (recur))))
        (let [ints (persistent! acc)
              [ev _] (decode-key ints)]
          ev))
      :else
      (let [[ev _] (decode-key [c])]
        ev))))

(deftype JLineTerminal [^org.jline.terminal.Terminal term resize-handler closed?]
  Terminal
  (t-size [_]
    {:rows (.getHeight term) :cols (.getWidth term)})
  (t-read-key [_]
    (read-key-from-reader (.reader term)))
  (t-write! [_ s]
    (.write (.writer term) ^String s))
  (t-flush! [_]
    (.flush (.writer term)))
  (t-set-cursor! [this x y visible?]
    (t-write! this (cursor-position-string x y))
    (t-write! this (if visible? ansi-cursor-show ansi-cursor-hide)))
  (t-enter! [this]
    (.enterRawMode term)
    (t-write! this ansi-alt-screen-enter)
    (t-write! this ansi-cursor-hide)
    (t-flush! this))
  (t-leave! [this]
    ;; Idempotent: `quit!` and the input loop's `finally` both call `t-leave!`, and on Ctrl-Q they
    ;; race on the same terminal. The first call restores the screen and closes the JLine terminal;
    ;; a second call must NOT touch it (writing to a closed terminal throws
    ;; `IllegalStateException: Terminal has been closed`). The CAS ensures only the first runs.
    (when (compare-and-set! closed? false true)
      (t-write! this ansi-cursor-show)
      (t-write! this ansi-alt-screen-leave)
      (t-flush! this)
      (.close term)))
  (t-sync-supported? [_]
    ;; best-effort: this JLine/terminfo build has no Sync capability enum, so report false.
    false)
  (t-on-resize! [_ handler]
    (reset! resize-handler handler)
    ;; Deliver terminal-resize (SIGWINCH) to the registered handler. We use `sun.misc.Signal`
    ;; instead of JLine's `(.handle term Terminal$Signal/WINCH ...)` because JLine's signal enum
    ;; and `SignalHandler` are INNER classes that babashka's SCI can neither resolve symbolically
    ;; nor `reify`. `sun.misc.Signal`/`SignalHandler` are top-level, so the SAME code runs on the
    ;; JVM and under bb. Installing here (after the terminal is built) also overrides the WINCH
    ;; handler JLine installs for itself. JLine/the OS deliver this on a separate signal thread, so
    ;; the handler must be safe to call concurrently with the input loop. Wrapped in try/catch:
    ;; WINCH is absent on some platforms (e.g. Windows), where resize signals are simply ignored.
    (try
      (sun.misc.Signal/handle
       (sun.misc.Signal. "WINCH")
       (reify sun.misc.SignalHandler
         (handle [_ _sig]
           (when-let [h @resize-handler] (h)))))
      (catch Throwable _ nil))
    nil))

(defn jline-terminal
  "Returns a `Terminal` backed by a system JLine terminal (`TerminalBuilder`)."
  []
  (let [term (.. (TerminalBuilder/builder) (system true) (build))]
    (->JLineTerminal term (atom nil) (atom false))))

;; =============================================================================
;; Fake terminal (string-terminal)
;; =============================================================================

(deftype StringTerminal [state]
  Terminal
  (t-size [_]
    (select-keys @state [:rows :cols]))
  (t-read-key [_]
    (let [k (-> @state :keys first)]
      (when k (swap! state update :keys (comp vec rest)))
      k))
  (t-write! [_ s]
    (swap! state update :output str s)
    nil)
  (t-flush! [_] nil)
  (t-set-cursor! [_ x y visible?]
    (swap! state assoc :cursor {:x x :y y :visible? visible?})
    nil)
  (t-enter! [_]
    (swap! state assoc :entered? true)
    nil)
  (t-leave! [_]
    (swap! state assoc :left? true)
    nil)
  (t-sync-supported? [_]
    (boolean (:sync? @state)))
  (t-on-resize! [_ handler]
    (swap! state assoc :on-resize handler)
    nil))

(defn string-terminal
  "Returns an atom-backed fake `Terminal` for testing. `opts`:

   * `:rows` - terminal height (default 24)
   * `:cols` - terminal width (default 80)
   * `:keys` - a seq of scripted `::key-event`s that `t-read-key` will dequeue, in order
   * `:sync?` - the boolean reported by `t-sync-supported?` (default false)

   Use the accessors `output`, `cursor`, `feed!`, and `resize!` to drive/inspect it."
  [{:keys [rows cols keys sync?] :or {rows 24 cols 80 keys []}}]
  (->StringTerminal (atom {:rows rows :cols cols :keys (vec keys)
                           :output "" :cursor nil :sync? (boolean sync?)})))

(defn output
  "Returns the accumulated string written to the fake terminal `t`."
  [^StringTerminal t]
  (:output @(.-state t)))

(defn cursor
  "Returns the last recorded cursor map `{:x :y :visible?}` for the fake terminal `t`, or nil."
  [^StringTerminal t]
  (:cursor @(.-state t)))

(defn feed!
  "Enqueues additional scripted key `events` onto the fake terminal `t`'s read queue."
  [^StringTerminal t & events]
  (swap! (.-state t) update :keys into events)
  nil)

(defn resize!
  "Sets the fake terminal `t` dimensions to `rows` x `cols`, then invokes its registered resize handler
   (see `t-on-resize!`), if any — simulating a real terminal's SIGWINCH delivery."
  [^StringTerminal t rows cols]
  (swap! (.-state t) assoc :rows rows :cols cols)
  (when-let [h (:on-resize @(.-state t))] (h))
  nil)
