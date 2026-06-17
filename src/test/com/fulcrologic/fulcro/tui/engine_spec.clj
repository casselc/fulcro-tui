(ns com.fulcrologic.fulcro.tui.engine-spec
  (:require
    [clojure.string :as str]
    [com.fulcrologic.fulcro.algorithms.tx-processing.synchronous-tx-processing :as stx]
    [com.fulcrologic.fulcro.components :as comp]
    [com.fulcrologic.fulcro.mutations :as m]
    [com.fulcrologic.fulcro.raw.application :as rapp]
    [com.fulcrologic.fulcro.raw.components :as rc]
    [com.fulcrologic.fulcro.tui.elements :as elements]
    [com.fulcrologic.fulcro.tui.engine :as engine]
    [fulcro-spec.core :refer [=> =throws=> assertions component specification]]))

;; ---------------------------------------------------------------------------
;; Component fixtures for the walker / utility specs (standard Fulcro defsc;
;; their render returns TUI nodes, driven by engine/render-tree instead of React)
;; ---------------------------------------------------------------------------

(comp/defsc Leaf [this {:keys [leaf/id leaf/label]} {:keys [on-select]}]
  {:query [:leaf/id :leaf/label]
   :ident :leaf/id}
  (elements/button {:id (str "leaf-" id) :on-activate on-select} label))

(def ui-leaf (comp/computed-factory Leaf))

(comp/defsc Container [this {:keys [c/title c/leaf]}]
  {:query [:c/title {:c/leaf (rc/get-query Leaf)}]
   :ident :c/title}
  (elements/vbox {:id "root"}
    (elements/text {:id "title"} title)
    (ui-leaf leaf {:on-select (fn [] :selected)})))

(comp/defsc Plain [this {:keys [p/id p/label]}]
  {:query [:p/id :p/label]
   :ident :p/id}
  (elements/button {:id (str "plain-" id)} label))

(comp/defsc Multi [this {:keys [m/id]}]
  {:query [:m/id]
   :ident :m/id}
  [(elements/text {} "a") (elements/text {} "b")])

(specification {:covers {`engine/node? "863300"}} "node?"
  (assertions
    "is true for a node produced by a generator"
    (engine/node? (elements/vbox {})) => true
    "is false for a plain map without a tag"
    (engine/node? {:a 1}) => false
    "is false for non-map values"
    (engine/node? "x") => false
    (engine/node? nil) => false))

(specification {:covers {`engine/code-point-width "bb5a0d,d044cd"}} "code-point-width"
  (assertions
    "is 1 for an ordinary ASCII letter"
    (engine/code-point-width (int \A)) => 1
    "is 0 for control characters (newline)"
    (engine/code-point-width (int \newline)) => 0
    "is 0 for a combining mark"
    (engine/code-point-width 0x0301) => 0
    "is 2 for a CJK ideograph"
    (engine/code-point-width 0x4E00) => 2
    "is 2 for a fullwidth form"
    (engine/code-point-width 0xFF21) => 2
    "is 2 for an emoji"
    (engine/code-point-width 0x1F600) => 2))

(specification {:covers {`engine/string-width "6aa7c8,398b8d"}} "string-width"
  (assertions
    "counts one column per ASCII character"
    (engine/string-width "hello") => 5
    "counts wide characters as two columns"
    (engine/string-width "ab一") => 4
    "ignores zero-width combining marks (precomposed or decomposed)"
    (engine/string-width "áb") => 2
    "is zero for the empty string"
    (engine/string-width "") => 0))

(specification {:covers {`engine/wrap-text "d41206,bbb898"}} "wrap-text"
  (component "short text fits on one line"
    (assertions
      "text narrower than the width is a single line"
      (engine/wrap-text "hello" 10) => ["hello"]
      "text exactly the width is a single line"
      (engine/wrap-text "hello" 5) => ["hello"]))

  (component "greedy word wrapping"
    (assertions
      "packs words onto a line until the next word would overflow, then breaks"
      (engine/wrap-text "the quick brown fox" 9) => ["the quick" "brown fox"]
      "puts each word on its own line when only one fits"
      (engine/wrap-text "aaa bbb ccc" 3) => ["aaa" "bbb" "ccc"]))

  (component "over-long words"
    (assertions
      "hard-breaks a single word wider than the width into width-sized pieces"
      (engine/wrap-text "abcdefgh" 3) => ["abc" "def" "gh"]
      "breaks the long head then continues wrapping the remaining words"
      (engine/wrap-text "supercalifragilistic and more" 10)
      => ["supercalif" "ragilistic" "and more"]))

  (component "embedded newlines"
    (assertions
      "splits on hard newlines before word-wrapping each segment"
      (engine/wrap-text "line1\nline2" 10) => ["line1" "line2"]
      "a blank segment between newlines yields an empty visual line"
      (engine/wrap-text "a\n\nb" 10) => ["a" "" "b"]))

  (component "spaces and empties"
    (assertions
      "preserves runs of spaces that fit within a line"
      (engine/wrap-text "a  b c" 10) => ["a  b c"]
      "the empty string is one empty line"
      (engine/wrap-text "" 5) => [""]))

  (component "wide characters measured by display width"
    (assertions
      "hard-breaks wide (2-column) characters without splitting one across the boundary"
      (engine/wrap-text "一二三 abc" 4) => ["一二" "三" "abc"]))

  (component "non-positive width disables wrapping"
    (assertions
      "width 0 returns each newline segment as one line"
      (engine/wrap-text "x y z" 0) => ["x y z"]
      (engine/wrap-text "a b\nc d" 0) => ["a b" "c d"])))

(specification {:covers {`engine/wrapping-text? "e01c56,ccafd0"}} "wrapping-text?"
  (assertions
    "is true for a :text node with :wrap true"
    (engine/wrapping-text? (elements/text {:wrap true} "x")) => true
    "is false for a :text node without :wrap"
    (engine/wrapping-text? (elements/text {} "x")) => false
    "is false for a non-text node even with :wrap true"
    (engine/wrapping-text? (elements/box {:wrap true})) => false
    "is false for a non-node"
    (engine/wrapping-text? "x") => false))

(defn- row0-underlined
  "Returns the seq of chars in row 0 of `buf` whose cell style has `:underline?` set."
  [buf cols]
  (->> (range cols)
    (keep (fn [c] (let [{:keys [ch sgr]} (get-in buf [:cells c])]
                    (when (:underline? sgr) ch))))))

(specification "style->sgr-codes underline"
  (assertions
    "maps :underline? to SGR code 4, after reverse/bold and before colors"
    (engine/style->sgr-codes {:underline? true}) => [4]
    (engine/style->sgr-codes {:bold? true :underline? true :fg :red}) => [1 4 31]
    "omits 4 when :underline? is falsey"
    (engine/style->sgr-codes {:fg :red}) => [31]))

(specification {:covers {`engine/chord-base-key "99c2c3"}} "mnemonic underline rendering"
  (component "a control's :shortcut underlines the matching label letter when enhanced keys are active"
    (let [btn (engine/place (elements/button {:id :save :shortcut [:alt "s"]} "Save") {:x 0 :y 0 :w 10 :h 1})
          on  (binding [engine/*enhanced-keys?* true]  (engine/render-buffer btn 1 10))
          off (binding [engine/*enhanced-keys?* false] (engine/render-buffer btn 1 10))]
      (assertions
        "the first matching label cell is underlined under the enhanced protocol"
        (row0-underlined on 10) => [\S]
        "nothing is underlined when the enhanced protocol is inactive"
        (row0-underlined off 10) => []
        "the label text itself is unchanged"
        (str/trim (first (engine/screen on))) => "Save")))

  (component "whole-node :underline underlines every cell"
    (let [txt (engine/place (elements/text {:underline true} "Hi") {:x 0 :y 0 :w 4 :h 1})
          buf (engine/render-buffer txt 1 4)]
      (assertions
        "both characters carry the underline attribute"
        (row0-underlined buf 4) => [\H \i])))

  (component "a special-key chord produces no mnemonic underline"
    (let [btn (engine/place (elements/button {:id :go :shortcut :f2} "Go") {:x 0 :y 0 :w 6 :h 1})
          buf (binding [engine/*enhanced-keys?* true] (engine/render-buffer btn 1 6))]
      (assertions
        "there is no single letter to underline for :f2"
        (row0-underlined buf 6) => []))))

(specification "wrapping text layout (height-for-width)"
  (component "place in a vbox: height is the wrapped line count at the container width"
    (let [placed (engine/place (elements/vbox {} (elements/text {:wrap true} "the quick brown fox jumps"))
                   {:x 0 :y 0 :w 9 :h 10})
          child  (first (::engine/children placed))]
      (assertions
        "the wrapping text fills the container width on the cross axis"
        (:w (::engine/rect child)) => 9
        "its height equals the number of wrapped lines at that width"
        (:h (::engine/rect child)) => 3
        (:h (::engine/rect child)) => (count (engine/wrap-text "the quick brown fox jumps" 9)))))

  (component "render-buffer paints the wrapped lines into the rect (clipped)"
    (let [placed (engine/place (elements/vbox {} (elements/text {:wrap true} "the quick brown fox jumps"))
                   {:x 0 :y 0 :w 9 :h 10})
          buf    (engine/render-buffer placed 4 9)]
      (assertions
        "successive wrapped lines render on successive rows"
        (take 3 (engine/screen buf)) => ["the quick" "brown fox" "jumps    "])))

  (component "a long wrapped paragraph inside a viewport wraps and scrolls"
    (let [para   "the quick brown fox jumps over the lazy dog"
          placed (engine/place (elements/viewport {:id :vp :height 3} (elements/text {:wrap true} para))
                   {:x 0 :y 0 :w 9 :h 3})]
      (assertions
        "the viewport's virtual height is the wrapped line count at its content width"
        (:h (::engine/virtual-size placed)) => (count (engine/wrap-text para 9))
        "with scroll 0 the first wrapped lines are shown"
        (engine/screen (engine/render-buffer (assoc placed ::engine/scroll {:x 0 :y 0}) 3 9))
        => ["the quick" "brown fox" "jumps    "]
        "scrolling down reveals deeper wrapped lines"
        (engine/screen (engine/render-buffer (assoc placed ::engine/scroll {:x 0 :y 2}) 3 9))
        => ["jumps    " "over the " "lazy dog "]))))

(specification {:covers {`engine/intrinsic-size "285e13,d2a4af"}} "intrinsic-size"
  (component "leaves"
    (assertions
      "text is as wide as its longest line and as tall as its line count"
      (engine/intrinsic-size (elements/text {} "ab\ncde")) => {:w 3 :h 2}
      "single-line text is one row tall"
      (engine/intrinsic-size (elements/text {} "hi")) => {:w 2 :h 1}
      "a button is as wide as its label, one row tall"
      (engine/intrinsic-size (elements/button {} "OK")) => {:w 2 :h 1}
      "an input is as wide as its value"
      (engine/intrinsic-size (elements/input {:value "abc"})) => {:w 3 :h 1}
      "an empty input is at least one column wide"
      (engine/intrinsic-size (elements/input {})) => {:w 1 :h 1}
      "a viewport takes its declared fixed size"
      (engine/intrinsic-size (elements/viewport {:width 20 :height 5})) => {:w 20 :h 5}))

  (component "containers"
    (assertions
      "a vbox is as wide as its widest child and as tall as the sum of child heights"
      (engine/intrinsic-size (elements/vbox {} (elements/text {} "ab") (elements/text {} "cde"))) => {:w 3 :h 2}
      "an hbox is as wide as the sum of child widths and as tall as its tallest child"
      (engine/intrinsic-size (elements/hbox {} (elements/text {} "ab") (elements/text {} "cde"))) => {:w 5 :h 1}))

  (component "insets and overrides"
    (assertions
      "a border adds one cell on every edge"
      (engine/intrinsic-size (elements/box {:border? true} (elements/text {} "hi"))) => {:w 4 :h 3}
      "padding adds its value on every edge"
      (engine/intrinsic-size (elements/box {:padding 2} (elements/text {} "hi"))) => {:w 6 :h 5}
      "a fixed :width overrides the content width"
      (engine/intrinsic-size (elements/text {:width 8} "hi")) => {:w 8 :h 1}
      "min-width floors the width (winning over a smaller fixed width)"
      (engine/intrinsic-size (elements/text {:width 8 :min-width 10} "hi")) => {:w 10 :h 1})))

(defn- child-rects
  "Returns the vector of child ::rect maps of a placed node."
  [placed]
  (mapv ::engine/rect (::engine/children placed)))

(specification {:covers {`engine/place "3667ed,69acf7"}} "place"
  (component "the node's own rect"
    (assertions
      "is the outer rect it was placed in"
      (::engine/rect (engine/place (elements/vbox {}) {:x 2 :y 3 :w 8 :h 4})) => {:x 2 :y 3 :w 8 :h 4}))

  (component "vbox stacking (vertical)"
    (assertions
      "places fixed-height children sequentially, filling width on the cross axis"
      (child-rects (engine/place (elements/vbox {} (elements/text {:height 1} "a") (elements/text {:height 2} "b"))
                     {:x 0 :y 0 :w 10 :h 4}))
      => [{:x 0 :y 0 :w 10 :h 1} {:x 0 :y 1 :w 10 :h 2}]
      "gives a :grow child the height left over after a fixed child"
      (child-rects (engine/place (elements/vbox {} (elements/text {:height 1} "h") (elements/text {:grow 1} "body"))
                     {:x 0 :y 0 :w 6 :h 5}))
      => [{:x 0 :y 0 :w 6 :h 1} {:x 0 :y 1 :w 6 :h 4}]
      "splits leftover height evenly between two equal-weight grow children"
      (mapv :h (child-rects (engine/place (elements/vbox {} (elements/text {:grow 1} "a") (elements/text {:grow 1} "b"))
                              {:x 0 :y 0 :w 4 :h 4})))
      => [2 2]))

  (component "hbox stacking (horizontal)"
    (assertions
      "gives a fixed-width sidebar its width and the rest to a grow child"
      (mapv :w (child-rects (engine/place (elements/hbox {} (elements/box {:width 3}) (elements/box {:grow 1}))
                              {:x 0 :y 0 :w 10 :h 2})))
      => [3 7]
      "splits width between two :half children"
      (mapv :w (child-rects (engine/place (elements/hbox {} (elements/box {:width :half}) (elements/box {:width :half}))
                              {:x 0 :y 0 :w 10 :h 2})))
      => [5 5]))

  (component "grow distribution yields whole-cell sizes (no fractional coordinates)"
    (assertions
      ;; A SINGLE grow child already got an integer (it takes the leftover remainder); the bug only bit
      ;; with TWO+ grow children, where the non-last ones go through (quot (* leftover w) total-w) — and
      ;; quot of DOUBLES returns a double, leaking floats into the placed rects.
      "two float-weighted :grow children split the leftover into integers, not doubles"
      (let [ws (mapv :w (child-rects (engine/place
                                       (elements/hbox {} (elements/box {:grow 1.0}) (elements/box {:grow 1.0}))
                                       {:x 0 :y 0 :w 11 :h 1})))]
        [ws (mapv integer? ws)])
      => [[5 6] [true true]]
      "unequal float weights still partition the whole extent as integers, remainder to the last"
      (mapv :w (child-rects (engine/place
                              (elements/hbox {} (elements/box {:grow 1.0}) (elements/box {:grow 2.0}))
                              {:x 0 :y 0 :w 10 :h 1})))
      => [3 7]
      "placed x-coords stay integers with multiple float-weighted grow children"
      (mapv :x (child-rects (engine/place
                              (elements/hbox {} (elements/box {:grow 1.0}) (elements/text {} "x") (elements/box {:grow 1.0}))
                              {:x 0 :y 0 :w 30 :h 1})))
      => [0 14 15]
      ;; Regression: before the fix, the double coordinates made put-cell's vector assoc throw
      ;; \"IllegalArgumentException: Key must be integer\" during paint.
      "a tree with multiple float-weighted :grow children renders without throwing"
      (let [row (first (engine/screen (engine/render-buffer
                                        (engine/place
                                          (elements/hbox {} (elements/box {:grow 1.0}) (elements/text {} "x") (elements/box {:grow 1.0}))
                                          {:x 0 :y 0 :w 30 :h 1})
                                        1 30)))]
        [(count row) (str/index-of row "x")])
      => [30 14]))

  (component "cross-axis alignment"
    (assertions
      "centers a narrower child within the container width"
      (::engine/rect (first (::engine/children
                              (engine/place (elements/vbox {} (elements/text {:width 4 :align :center} "x"))
                                {:x 0 :y 0 :w 10 :h 1}))))
      => {:x 3 :y 0 :w 4 :h 1}))

  (component "main-axis justify"
    (let [xs (fn [w attrs]
               ;; three 2-wide boxes (6 used) in a w-wide hbox => (w - 6) cells of slack to distribute
               (mapv :x (child-rects
                          (engine/place
                            (elements/hbox attrs
                              (elements/box {:width 2}) (elements/box {:width 2}) (elements/box {:width 2}))
                            {:x 0 :y 0 :w w :h 1}))))
          x1 (fn [attrs]
               ;; a single 2-wide box in an 18-wide hbox => 16 cells of slack (exercises the n=1 paths)
               (-> (engine/place (elements/hbox attrs (elements/box {:width 2})) {:x 0 :y 0 :w 18 :h 1})
                 ::engine/children first ::engine/rect :x))]
      (assertions
        ;; w=18 => 12 cells of slack, evenly divisible by 2/3/4 (clean gaps, no rounding)
        "defaults to :start — children packed at the near edge, leftover unused (unchanged behavior)"
        (xs 18 {}) => [0 2 4]
        "an explicit :start matches the default"
        (xs 18 {:justify :start}) => [0 2 4]
        ":center centers the packed block of children"
        (xs 18 {:justify :center}) => [6 8 10]
        ":end pushes the packed block to the far edge"
        (xs 18 {:justify :end}) => [12 14 16]
        ":space-between spreads the slack into the gaps between children, none at the ends"
        (xs 18 {:justify :space-between}) => [0 8 16]
        ":space-around surrounds each child with equal space (half-size at the ends)"
        (xs 18 {:justify :space-around}) => [2 8 14]
        ":space-evenly makes every gap, including the ends, equal"
        (xs 18 {:justify :space-evenly}) => [3 8 13]

        ;; w=17 => 11 cells of slack: NOT divisible, so quot truncation drops a cell or two (pin rounding)
        ":end always reaches the far edge even with an odd remainder (lead = whole slack)"
        (xs 17 {:justify :end}) => [11 13 15]
        ":space-between truncates the gap via quot; the last child need not reach the far edge"
        (xs 17 {:justify :space-between}) => [0 7 14]
        ":space-around truncates via quot with an odd remainder"
        (xs 17 {:justify :space-around}) => [1 6 11]
        ":space-evenly truncates via quot with an odd remainder"
        (xs 17 {:justify :space-evenly}) => [2 6 10]

        ;; single child (n=1): :space-between must fall back to :start (guards (dec n)=0 divide-by-zero)
        ":center moves a lone child to the middle"
        (x1 {:justify :center}) => 8
        ":end moves a lone child to the far edge"
        (x1 {:justify :end}) => 16
        ":space-between falls back to :start for a lone child (no divide-by-zero on (dec n))"
        (x1 {:justify :space-between}) => 0
        ":space-around centers a lone child"
        (x1 {:justify :space-around}) => 8
        ":space-evenly centers a lone child"
        (x1 {:justify :space-evenly}) => 8))
    (assertions
      "an empty justified container places no children (n=0 guard; no divide-by-zero in space-around)"
      (child-rects (engine/place (elements/hbox {:justify :space-around}) {:x 0 :y 0 :w 18 :h 1})) => []
      "is inert when a :grow child already absorbs the leftover space"
      (child-rects (engine/place
                     (elements/hbox {:justify :center}
                       (elements/box {:width 2}) (elements/box {:grow 1}) (elements/box {:width 2}))
                     {:x 0 :y 0 :w 18 :h 1}))
      => [{:x 0 :y 0 :w 2 :h 1} {:x 2 :y 0 :w 14 :h 1} {:x 16 :y 0 :w 2 :h 1}]
      "applies on the main (vertical) axis of a vbox too"
      (mapv :y (child-rects (engine/place
                              (elements/vbox {:justify :end}
                                (elements/box {:height 2}) (elements/box {:height 2}))
                              {:x 0 :y 0 :w 4 :h 12})))
      => [8 10]
      "applies to a :modal's vertically-stacked children too"
      (mapv :y (child-rects (engine/place
                              (elements/modal {:border? false :justify :end}
                                (elements/box {:height 2}) (elements/box {:height 2}))
                              {:x 0 :y 0 :w 10 :h 12})))
      => [8 10]))

  (component "insets"
    (assertions
      "a border insets the content area by one cell on each edge"
      (child-rects (engine/place (elements/vbox {:border? true} (elements/text {} "x"))
                     {:x 0 :y 0 :w 5 :h 3}))
      => [{:x 1 :y 1 :w 3 :h 1}]))

  (component "viewport"
    (let [tall   (elements/vbox {} (elements/text {} "L0") (elements/text {} "L1") (elements/text {} "L2")
                   (elements/text {} "L3") (elements/text {} "L4"))
          placed (engine/place (elements/viewport {:id :vp :height 3} tall) {:x 0 :y 0 :w 4 :h 3})]
      (assertions
        "lays its single child out at the child's NATURAL height (taller than the viewport)"
        (::engine/rect (::engine/viewport-content placed)) => {:x 0 :y 0 :w 4 :h 5}
        "records the virtual content size (content width x natural height)"
        (::engine/virtual-size placed) => {:w 4 :h 5}
        "places the virtual child in 0-based virtual coordinates, not absolute screen coords"
        (mapv ::engine/rect (::engine/children (::engine/viewport-content placed)))
        => [{:x 0 :y 0 :w 4 :h 1} {:x 0 :y 1 :w 4 :h 1} {:x 0 :y 2 :w 4 :h 1}
            {:x 0 :y 3 :w 4 :h 1} {:x 0 :y 4 :w 4 :h 1}]
        "attaches a default scroll offset of {0,0}"
        (::engine/scroll placed) => {:x 0 :y 0}
        "the viewport's own outer rect is the rect it was placed in"
        (::engine/rect placed) => {:x 0 :y 0 :w 4 :h 3}))))

(specification {:covers {`engine/clamp-scroll "cd8b2d"}} "clamp-scroll"
  (assertions
    "clamps a too-small (negative) offset up to 0 on both axes"
    (engine/clamp-scroll {:x -3 :y -2} {:w 10 :h 10} {:w 4 :h 4}) => {:x 0 :y 0}
    "clamps a too-large offset down to (virtual - view) on both axes"
    (engine/clamp-scroll {:x 99 :y 99} {:w 10 :h 10} {:w 4 :h 4}) => {:x 6 :y 6}
    "leaves an in-range offset unchanged"
    (engine/clamp-scroll {:x 2 :y 3} {:w 10 :h 10} {:w 4 :h 4}) => {:x 2 :y 3}
    "clamps to zero on an axis whose virtual size is no larger than the view"
    (engine/clamp-scroll {:x 5 :y 5} {:w 4 :h 4} {:w 4 :h 4}) => {:x 0 :y 0}
    (engine/clamp-scroll {:x 5 :y 5} {:w 3 :h 3} {:w 4 :h 4}) => {:x 0 :y 0}))

(specification {:covers {`engine/scroll-to-show "8c2a68"}} "scroll-to-show"
  (assertions
    "scrolls forward minimally so a below-window rect's far edge is the last visible cell"
    (engine/scroll-to-show {:x 0 :y 0} {:x 0 :y 4 :w 4 :h 1} {:w 4 :h 3}) => {:x 0 :y 2}
    "scrolls back to a rect's near edge when it is above the window"
    (engine/scroll-to-show {:x 0 :y 2} {:x 0 :y 0 :w 4 :h 1} {:w 4 :h 3}) => {:x 0 :y 0}
    "leaves scroll unchanged when the rect is already fully visible"
    (engine/scroll-to-show {:x 0 :y 2} {:x 0 :y 3 :w 4 :h 1} {:w 4 :h 3}) => {:x 0 :y 2}
    "shows the near edge of a rect taller than the window"
    (engine/scroll-to-show {:x 0 :y 0} {:x 0 :y 1 :w 4 :h 5} {:w 4 :h 3}) => {:x 0 :y 1}
    "adjusts both axes independently"
    (engine/scroll-to-show {:x 0 :y 0} {:x 5 :y 4 :w 1 :h 1} {:w 3 :h 3}) => {:x 3 :y 2}))

(specification {:covers {`engine/viewport? "014fb0,ccafd0"}} "viewport?"
  (assertions
    "is true for a viewport node"
    (engine/viewport? (elements/viewport {})) => true
    "is false for other node tags"
    (engine/viewport? (elements/box {})) => false
    "is false for non-nodes"
    (engine/viewport? "x") => false))

(specification {:covers {`engine/content-view-size "9380cf"}} "content-view-size"
  (assertions
    "is the rect size when there are no insets"
    (engine/content-view-size (engine/place (elements/viewport {}) {:x 0 :y 0 :w 8 :h 4})) => {:w 8 :h 4}
    "shrinks by one cell per edge for a border"
    (engine/content-view-size (engine/place (elements/viewport {:border? true}) {:x 0 :y 0 :w 8 :h 4})) => {:w 6 :h 2}
    "shrinks by the padding on every edge"
    (engine/content-view-size (engine/place (elements/viewport {:padding 1}) {:x 0 :y 0 :w 8 :h 4})) => {:w 6 :h 2}))

(specification {:covers {`engine/placed-viewports       "ade25c,203acc"
                         `engine/focus-viewport-context "e6a13e,72aaa9"}} "placed-viewports / focus-viewport-context"
  (let [tree   (elements/vbox {:id "root"}
                 (elements/text {} "hdr")
                 (elements/viewport {:id :vp}
                   (elements/vbox {} (elements/button {:id :i0} "I0") (elements/button {:id :i1} "I1")
                     (elements/button {:id :i2} "I2") (elements/button {:id :i3} "I3"))))
        placed (engine/place tree {:x 0 :y 0 :w 6 :h 3})]
    (component "placed-viewports"
      (assertions
        "finds the viewport node in the placed tree"
        (mapv #(engine/node-attr % :id) (engine/placed-viewports placed)) => [:vp]))
    (component "focus-viewport-context"
      (assertions
        "identifies the enclosing viewport of a focused node"
        (engine/node-attr (:viewport (engine/focus-viewport-context placed :i2)) :id) => :vp
        "returns the focused node's VIRTUAL rect (0-based within the viewport)"
        (:virtual-rect (engine/focus-viewport-context placed :i2)) => {:x 0 :y 2 :w 6 :h 1}
        (:virtual-rect (engine/focus-viewport-context placed :i0)) => {:x 0 :y 0 :w 6 :h 1}
        "returns nil for an id that is not inside any viewport"
        (engine/focus-viewport-context placed :nope) => nil))))

(specification {:covers {`engine/viewport-scroll      "c571ab,797b46"
                         `engine/all-scroll           "68fe31"
                         `engine/set-viewport-scroll! "60901d"}} "viewport scroll state"
  (let [state-atom (atom {})
        app        {:com.fulcrologic.fulcro.application/state-atom state-atom}]
    (assertions
      "viewport-scroll defaults to {0,0} when none is recorded"
      (engine/viewport-scroll app :vp) => {:x 0 :y 0}
      "all-scroll is empty when none is recorded"
      (engine/all-scroll app) => {})
    (engine/set-viewport-scroll! app :vp {:x 1 :y 4})
    (assertions
      "set-viewport-scroll! records the offset for that viewport id"
      (engine/viewport-scroll app :vp) => {:x 1 :y 4}
      "all-scroll returns the whole {id {:x :y}} map"
      (engine/all-scroll app) => {:vp {:x 1 :y 4}}
      "viewport-scroll reads from a raw state-map too"
      (engine/viewport-scroll (deref state-atom) :vp) => {:x 1 :y 4})))

;; ============================================================================
;; Render
;; ============================================================================

(specification {:covers {`engine/make-buffer "d01cec,26290c"}} "make-buffer"
  (let [b (engine/make-buffer 2 3)]
    (assertions
      "has the requested row count"
      (:rows b) => 2
      "has the requested column count"
      (:cols b) => 3
      "holds rows*cols cells row-major"
      (count (:cells b)) => 6
      "fills every cell with a space in the default (empty) style"
      (set (:cells b)) => #{{:ch \space :sgr {}}})))

(specification {:covers {`engine/put-cell "990a89,71d211"}} "put-cell"
  (let [b (engine/make-buffer 2 3)]
    (assertions
      "sets the character and style at the given 0-based (x,y)"
      (engine/screen (engine/put-cell b 1 0 \X {:fg :red})) => [" X " "   "]
      "stores the style on the targeted cell"
      (:sgr (get-in (engine/screen-styled (engine/put-cell b 1 0 \X {:fg :red})) [0 1])) => {:fg :red}
      "indexes y as the row and x as the column"
      (engine/screen (engine/put-cell b 0 1 \Z {})) => ["   " "Z  "]
      "ignores writes outside the buffer bounds (clipped), returning the buffer unchanged"
      (engine/put-cell b 9 9 \X {}) => b
      (engine/put-cell b -1 0 \X {}) => b)))

(specification {:covers {`engine/put-str "aba056,b8f133"}} "put-str"
  (let [b    (engine/make-buffer 1 6)
        full {:x 0 :y 0 :w 6 :h 1}]
    (component "advancing and styling"
      (assertions
        "writes a string starting at (x,y)"
        (engine/screen (engine/put-str b 2 0 "hi" {} full)) => ["  hi  "]
        "styles every written cell"
        (mapv :sgr (subvec (first (engine/screen-styled (engine/put-str b 0 0 "ab" {:fg :red} full))) 0 2))
        => [{:fg :red} {:fg :red}]
        "advances a wide (2-column) character by two columns"
        (engine/screen (engine/put-str b 0 0 "一b" {} full)) => ["一b   "]))

    (component "clipping"
      (assertions
        "truncates text that runs past the clip rect width"
        (engine/screen (engine/put-str b 0 0 "hello" {} {:x 0 :y 0 :w 2 :h 1})) => ["he    "]
        "truncates text that runs past the buffer bounds"
        (engine/screen (engine/put-str b 4 0 "hello" {} full)) => ["    he"]
        "does not write a wide char whose second cell falls outside the clip"
        (engine/screen (engine/put-str b 0 0 "一" {} {:x 0 :y 0 :w 1 :h 1})) => ["      "]))))

(specification {:covers {`engine/style->sgr-codes "d154c2,851723"}} "style->sgr-codes"
  (assertions
    "returns an empty vector for the default style"
    (engine/style->sgr-codes {}) => []
    "maps :color/:fg to its palette code"
    (engine/style->sgr-codes {:fg :red}) => [31]
    "maps a bright color to its 90s code"
    (engine/style->sgr-codes {:fg :bright-white}) => [97]
    "maps :bg to the foreground code plus 10"
    (engine/style->sgr-codes {:bg :blue}) => [44]
    "emits reverse (7) and bold (1) before the colors, in that order"
    (engine/style->sgr-codes {:reverse? true :bold? true :fg :red :bg :blue}) => [7 1 31 44]))

(specification {:covers {`engine/sgr-string "0edb71"}} "sgr-string"
  (assertions
    "joins codes with semicolons inside a CSI ...m sequence"
    (engine/sgr-string [1 31]) => "[1;31m"
    "emits a single code with no separators"
    (engine/sgr-string [44]) => "[44m"
    "yields the reset sequence for an empty codes vector"
    (engine/sgr-string []) => "[0m"))

(specification {:covers {`engine/render-buffer "e06fec,fce04b"
                         `engine/paint         "643160,702f9c"}} "render-buffer / paint"
  (component "stacked leaves"
    (let [tree (engine/place (elements/vbox {}
                               (elements/text {} "Hello")
                               (elements/button {:highlight true} "OK")
                               (elements/line {}))
                 {:x 0 :y 0 :w 6 :h 3})
          buf  (engine/render-buffer tree 3 6)]
      (assertions
        "paints text, a button label, and a horizontal rule on successive rows"
        (engine/screen buf) => ["Hello " "OK    " "──────"]
        "renders a :highlight button cell with reverse video"
        (:sgr (get-in (engine/screen-styled buf) [1 0])) => {:reverse? true})))

  (component "color and bold styling"
    (let [buf (engine/render-buffer
                (engine/place (elements/text {:color :red :bold true} "X") {:x 0 :y 0 :w 3 :h 1}) 1 3)]
      (assertions
        "applies :color as a foreground and :bold as bold to text cells"
        (:sgr (get-in (engine/screen-styled buf) [0 0])) => {:fg :red :bold? true})))

  (component "input value"
    (let [buf (engine/render-buffer (engine/place (elements/input {:value "hi"}) {:x 0 :y 0 :w 4 :h 1}) 1 4)]
      (assertions
        "writes the input's :value into its rect"
        (engine/screen buf) => ["hi  "])))

  (component "vertical line"
    (let [buf (engine/render-buffer (engine/place (elements/line {}) {:x 0 :y 0 :w 1 :h 3}) 3 1)]
      (assertions
        "fills a taller-than-wide line rect with a vertical rule"
        (engine/screen buf) => ["│" "│" "│"])))

  (component "border drawing"
    (let [buf (engine/render-buffer (engine/place (elements/box {:border? true} (elements/text {} "x"))
                                      {:x 0 :y 0 :w 5 :h 3}) 3 5)]
      (assertions
        "draws box-drawing corners and edges around the outer rect with the child inside"
        (engine/screen buf) => ["┌───┐" "│x  │" "└───┘"])))

  (component "background fill"
    (let [buf (engine/render-buffer (engine/place (elements/box {:bg :blue :width 2 :height 1})
                                      {:x 0 :y 0 :w 2 :h 1}) 1 2)]
      (assertions
        "fills the content rect with the background style when :bg is set"
        (mapv :sgr (first (engine/screen-styled buf))) => [{:bg :blue} {:bg :blue}])))

  (component "child clipping to parent"
    (let [buf (engine/render-buffer
                (engine/place (elements/box {:width 3 :height 1} (elements/text {} "ABCDEFG"))
                  {:x 0 :y 0 :w 3 :h 1}) 1 6)]
      (assertions
        "a child cannot paint outside its parent's rect"
        (engine/screen buf) => ["ABC   "])))

  (component "viewport scrolling"
    (let [tall   (elements/vbox {} (elements/text {} "AA") (elements/text {} "BB") (elements/text {} "CC")
                   (elements/text {} "DD") (elements/text {} "EE"))
          placed (engine/place (elements/viewport {:id :vp :height 3} tall) {:x 0 :y 0 :w 2 :h 3})]
      (assertions
        "with scroll {:y 0} the top rows of the (taller) child are shown"
        (engine/screen (engine/render-buffer (assoc placed ::engine/scroll {:x 0 :y 0}) 3 2)) => ["AA" "BB" "CC"]
        "with scroll {:y k} the window starting at virtual row k is shown"
        (engine/screen (engine/render-buffer (assoc placed ::engine/scroll {:x 0 :y 2}) 3 2)) => ["CC" "DD" "EE"]
        "an out-of-range scroll is clamped so the last window is shown"
        (engine/screen (engine/render-buffer (assoc placed ::engine/scroll {:x 0 :y 99}) 3 2)) => ["CC" "DD" "EE"])))

  (component "viewport content is clipped to the viewport rect"
    (let [tall   (elements/vbox {} (elements/text {} "WIDE0") (elements/text {} "WIDE1") (elements/text {} "WIDE2")
                   (elements/text {} "WIDE3") (elements/text {} "WIDE4"))
          ;; viewport content area is 3 wide x 2 tall; place it offset into a larger buffer
          placed (engine/place (elements/viewport {:id :vp :width 3 :height 2} tall) {:x 1 :y 1 :w 3 :h 2})
          buf    (engine/render-buffer placed 4 6)]
      (assertions
        "rows above/below and columns left/right of the viewport stay blank (content cannot leak out)"
        (engine/screen buf) => ["      "
                                " WID  "
                                " WID  "
                                "      "])))

  (component "viewport border is drawn on the outer rect"
    (let [tall   (elements/vbox {} (elements/text {} "rr") (elements/text {} "ss") (elements/text {} "tt") (elements/text {} "uu"))
          placed (engine/place (elements/viewport {:id :vp :border? true} tall) {:x 0 :y 0 :w 4 :h 4})
          buf    (engine/render-buffer placed 4 4)]
      (assertions
        "draws box-drawing corners/edges around the viewport with the scrolled content inside"
        (engine/screen buf) => ["┌──┐" "│rr│" "│ss│" "└──┘"]
        "scrolling shows a deeper window inside the same border"
        (engine/screen (engine/render-buffer (assoc placed ::engine/scroll {:x 0 :y 2}) 4 4))
        => ["┌──┐" "│tt│" "│uu│" "└──┘"]))))

(specification {:covers {`engine/screen "f01462,64763b"}} "screen"
  (let [b (engine/make-buffer 2 3)]
    (assertions
      "returns one joined string per row"
      (engine/screen (engine/put-cell (engine/put-cell b 0 0 \a {}) 0 1 \b {})) => ["a  " "b  "]
      "drops the continuation cell that follows a wide character"
      (engine/screen (engine/put-str b 0 0 "一" {} {:x 0 :y 0 :w 3 :h 2})) => ["一 " "   "]
      "returns a blank string of spaces for an untouched row"
      (engine/screen b) => ["   " "   "])))

(specification {:covers {`engine/screen-styled "6390e9"}} "screen-styled"
  (let [b   (engine/make-buffer 1 2)
        out (engine/screen-styled (engine/put-cell b 0 0 \a {:fg :red}))]
    (assertions
      "returns one vector of cells per row"
      (count out) => 1
      "exposes the character of each cell"
      (mapv :ch (first out)) => [\a \space]
      "exposes the style of each cell"
      (mapv :sgr (first out)) => [{:fg :red} {}])))

(specification {:covers {`engine/diff "5b944f,410e72"}} "diff"
  (let [b1 (engine/make-buffer 1 5)
        ab (engine/put-str b1 0 0 "abc" {} {:x 0 :y 0 :w 5 :h 1})]
    (component "unchanged buffers"
      (assertions
        "produces no ops when the buffers are identical"
        (engine/diff ab ab) => []))

    (component "coalescing a run"
      (let [red-abc (engine/put-str b1 0 0 "abc" {:fg :red} {:x 0 :y 0 :w 5 :h 1})
            red-XYZ (engine/put-str b1 0 0 "XYZ" {:fg :red} {:x 0 :y 0 :w 5 :h 1})]
        (assertions
          "emits a single op for a run of adjacent changed cells that share a style"
          (engine/diff red-abc red-XYZ) => [{:row 0 :col 0 :sgr [31] :text "XYZ"}])))

    (component "non-adjacent changes"
      (let [xbz (engine/put-str b1 0 0 "xbz" {} {:x 0 :y 0 :w 5 :h 1})]
        (assertions
          "emits a separate op for each run separated by unchanged cells"
          (engine/diff ab xbz)
          => [{:row 0 :col 0 :sgr [] :text "x"} {:row 0 :col 2 :sgr [] :text "z"}])))

    (component "full repaint"
      (assertions
        "emits a full coalesced repaint of non-default cells when prev is nil"
        (engine/diff nil ab) => [{:row 0 :col 0 :sgr [] :text "abc"}]
        "emits a full repaint when prev has different dimensions"
        (engine/diff (engine/make-buffer 2 5) ab) => [{:row 0 :col 0 :sgr [] :text "abc"}]))))

(specification {:covers {`engine/ops->ansi "e8b518,3649a3"}} "ops->ansi"
  (assertions
    "emits a cursor move (row+1;col+1) then an SGR then the text"
    (engine/ops->ansi [{:row 1 :col 2 :sgr [31] :text "hi"}]) => "[2;3H[31mhi[0m"
    "suppresses a redundant SGR when the pen style is unchanged between ops"
    (engine/ops->ansi [{:row 0 :col 0 :sgr [31] :text "a"} {:row 0 :col 3 :sgr [31] :text "b"}])
    => "[1;1H[31ma[1;4Hb[0m"
    "emits a new SGR when the pen style changes between ops"
    (engine/ops->ansi [{:row 0 :col 0 :sgr [31] :text "a"} {:row 0 :col 3 :sgr [] :text "b"}])
    => "[1;1H[31ma[1;4H[0mb"
    "resets before a style that drops an attribute, so reverse video does not leak into the next run"
    (engine/ops->ansi [{:row 0 :col 0 :sgr [7] :text "a"} {:row 0 :col 3 :sgr [31] :text "b"}])
    => "[1;1H[7ma[1;4H[0m[31mb[0m"
    "does not reset when the new style only ADDS codes (output stays minimal)"
    (engine/ops->ansi [{:row 0 :col 0 :sgr [31] :text "a"} {:row 0 :col 3 :sgr [1 31] :text "b"}])
    => "[1;1H[31ma[1;4H[1;31mb[0m"
    "returns the empty string for no ops"
    (engine/ops->ansi []) => ""))

(specification {:covers {`engine/frame->ansi "9eafc2,c3ac74"}} "frame->ansi"
  (let [b1  (engine/make-buffer 1 5)
        nxt (engine/put-str b1 0 0 "X" {} {:x 0 :y 0 :w 5 :h 1})]
    (assertions
      "wraps the diff in DEC private mode 2026 when :sync? is true"
      (engine/frame->ansi b1 nxt {:sync? true}) => "[?2026h[1;1HX[?2026l"
      "does not wrap the diff when :sync? is false"
      (engine/frame->ansi b1 nxt {:sync? false}) => "[1;1HX")))

(specification {:covers {`engine/frame->ansi "9eafc2,c3ac74"}} "frame->ansi — clear-screen on resize"
  (let [b1  (engine/make-buffer 1 5)
        nxt (engine/put-str b1 0 0 "X" {} {:x 0 :y 0 :w 5 :h 1})]
    (assertions
      "prepends a clear-screen + cursor-home when :clear? is true (so a resize erases stale content)"
      (str/starts-with? (engine/frame->ansi b1 nxt {:clear? true}) "[2J[H") => true
      "emits no clear-screen when :clear? is false"
      (str/includes? (engine/frame->ansi b1 nxt {:clear? false}) "[2J") => false
      "emits no clear-screen when :clear? is absent"
      (str/includes? (engine/frame->ansi b1 nxt {}) "[2J") => false
      "places the clear INSIDE the sync wrapper when both are set (atomic clear+repaint)"
      (str/starts-with? (engine/frame->ansi b1 nxt {:sync? true :clear? true}) "[?2026h[2J") => true)))

;; ===========================================================================
;; Components & walker
;; ===========================================================================

;; TUI components are now standard Fulcro `defsc` components (no bespoke macro). These checks
;; confirm Fulcro's own compile-time validation still guards the query/ident invariants the TUI
;; fixtures rely on.
(specification "Fulcro defsc compile-time checks (used by TUI components)"
  (assertions
    "throws when a prop is destructured but is not in the :query"
    (comp/defsc* {} '(Bad [this {:keys [a/x a/y]}]
                       {:query [:a/x] :ident :a/x} nil))
    =throws=> clojure.lang.ExceptionInfo
    "throws when the :ident key is not in the :query"
    (comp/defsc* {} '(Bad2 [this {:keys [a/x]}]
                       {:query [:a/x] :ident :a/z} nil))
    =throws=> clojure.lang.ExceptionInfo
    "expands a valid form to a class definition"
    (let [expansion (comp/defsc* {} '(Ok [this {:keys [a/x]}]
                                       {:query [:a/x] :ident :a/x} nil))]
      (first expansion)) => 'do))

;; NOTE: component instances are built with Fulcro's own `comp/factory`/`comp/computed-factory`
;; (the TUI no longer wraps them — see tui.clj). Those are Fulcro's to cover; here we only verify
;; the TUI walker (`render-instance`/`render-tree`/`render-root`) consumes such instances.

(specification {:covers {`engine/render-instance "fa649a"}} "render-instance"
  (let [instance ((comp/factory Plain) {:p/id 3 :p/label "Click"})]
    (assertions
      "calls the class :render, returning its node with props flowing in"
      (engine/render-instance instance) => (elements/button {:id "plain-3"} "Click"))))

(specification {:covers {`engine/render-tree "eb9572,0da94c"}} "render-tree"
  (component "scalars and nil"
    (assertions
      "passes a string through unchanged"
      (engine/render-tree "hi") => "hi"
      "passes a number through unchanged"
      (engine/render-tree 42) => 42
      "drops nil"
      (engine/render-tree nil) => nil))
  (component "nodes"
    (assertions
      "keeps a leaf node's tag, attrs, and text"
      (engine/render-tree (elements/text {:id "t"} "x")) => (elements/text {:id "t"} "x")))
  (component "component instances"
    (let [tree (engine/render-root Container
                 {:c/title "Hello"
                  :c/leaf  {:leaf/id 1 :leaf/label "Press"}})]
      (assertions
        "produces a node tree for place"
        (engine/node? tree) => true
        "leaves no component instances among the children"
        (some rc/component-instance? (::engine/children tree)) => nil
        "splices a nested child component's rendered node into the parent's children"
        (-> tree ::engine/children second ::engine/tag) => :button
        (-> tree ::engine/children second ::engine/children) => ["Press"]
        "renders the parent's own text node"
        (-> tree ::engine/children first ::engine/children) => ["Hello"])))
  (component "vector render splices siblings"
    (let [tree (engine/render-tree ((comp/factory Multi) {:m/id 1}))]
      (assertions
        "a render returning a vector yields a vector of sibling nodes"
        tree => [(elements/text {} "a") (elements/text {} "b")]))))

(specification {:covers {`engine/render-root "f0c7e1,56c803"}} "render-root"
  (let [tree (engine/render-root Plain {:p/id 5 :p/label "Root"})]
    (assertions
      "builds the root instance via factory and walks it to a pure node tree"
      tree => (elements/button {:id "plain-5"} "Root"))))

(specification {:covers {`engine/node-attr "d53a76,ccafd0"}} "node-attr"
  (assertions
    "returns the value of an attribute on a node"
    (engine/node-attr (elements/text {:id "x" :color :red} "t") :color) => :red
    "returns nil for a missing attribute"
    (engine/node-attr (elements/text {} "t") :id) => nil
    "returns nil for a non-node value"
    (engine/node-attr "not-a-node" :id) => nil))

(specification {:covers {`engine/find-by-id "3fafe1,805ed0"}} "find-by-id"
  (let [tree (elements/vbox {:id "root"}
               (elements/text {:id "a"} "A")
               (elements/hbox {:id "mid"}
                 (elements/button {:id "target"} "hit")))]
    (assertions
      "finds a deeply nested node by its :id"
      (engine/find-by-id tree "target") => (elements/button {:id "target"} "hit")
      "returns the node itself when it matches"
      (engine/find-by-id tree "root") => tree
      "returns nil when no node has the id"
      (engine/find-by-id tree "nope") => nil)))

(specification {:covers {`engine/node-text "752bdf,ccafd0"}} "node-text"
  (let [tree (elements/vbox {}
               (elements/text {} "Hello ")
               (elements/hbox {} (elements/text {} "wor") "ld"))]
    (assertions
      "concatenates the text of a node and all its descendants"
      (engine/node-text tree) => "Hello world"
      "returns a bare string unchanged"
      (engine/node-text "abc") => "abc"
      "stringifies a bare number"
      (engine/node-text 42) => "42")))

(specification {:covers {`engine/activate! "99f954,805ed0"}} "activate!"
  (let [called (atom false)
        node   (elements/button {:id "go" :on-activate (fn [] (reset! called :yes))} "Go")]
    (assertions
      "invokes the node's :on-activate handler"
      (engine/activate! node) => :yes
      "the handler was actually run"
      (deref called) => :yes
      "returns nil when there is no handler"
      (engine/activate! (elements/button {} "x")) => nil)))

(specification {:covers {`engine/press! "f6f187,805ed0"}} "press!"
  (let [seen (atom nil)
        node (elements/box {:id "b" :on-key (fn [k] (reset! seen k))})]
    (assertions
      "invokes the node's :on-key handler with the key"
      (engine/press! node :enter) => :enter
      "the handler received the key"
      (deref seen) => :enter
      "returns nil when there is no handler"
      (engine/press! (elements/box {}) :x) => nil)))

(specification {:covers {`engine/type! "e574a3,805ed0"}} "type!"
  (let [typed (atom nil)
        node  (elements/input {:id "f" :value "" :on-change (fn [s] (reset! typed s))})]
    (assertions
      "invokes the node's :on-change handler with the proposed string"
      (engine/type! node "abc") => "abc"
      "the handler received the proposed string"
      (deref typed) => "abc"
      "returns nil when there is no handler"
      (engine/type! (elements/input {:value ""}) "z") => nil)))

;; ===========================================================================
;; Focus, input & key dispatch
;; ===========================================================================

(specification {:covers {`engine/wrap-layout "3bcd5a,7d5d7b"}} "wrap-layout"
  (assertions
    "records each wrapped row's text, start caret index, and consumed length"
    (engine/wrap-layout "the quick brown fox" 9)
    => [{:start 0 :len 9 :text "the quick"} {:start 10 :len 9 :text "brown fox"}]
    "advances :start across the dropped soft-wrap space (caret 9 is the gap)"
    (mapv :start (engine/wrap-layout "the quick brown fox" 9)) => [0 10]
    "advances :start across a hard newline"
    (engine/wrap-layout "a\nbb\nccc" 9)
    => [{:start 0 :len 1 :text "a"} {:start 2 :len 2 :text "bb"} {:start 5 :len 3 :text "ccc"}]
    "yields a single empty row for the empty string"
    (engine/wrap-layout "" 9) => [{:start 0 :len 0 :text ""}]
    "represents a blank line between newlines as an empty row at the right caret index"
    (engine/wrap-layout "a\n\nb" 9) => [{:start 0 :len 1 :text "a"} {:start 2 :len 0 :text ""} {:start 3 :len 1 :text "b"}]
    "puts a trailing newline's empty line after the content"
    (engine/wrap-layout "ab\n" 9) => [{:start 0 :len 2 :text "ab"} {:start 3 :len 0 :text ""}]))

(specification {:covers {`engine/caret->rowcol "17bca2,5ccf39"
                         `engine/rowcol->caret "2b265f,5ccf39"}} "caret <-> rowcol"
  (let [v "the quick brown fox"]                            ; wraps at 9 to ["the quick" "brown fox"]
    (component "caret->rowcol"
      (assertions
        "maps the start of the value to [0 0]"
        (engine/caret->rowcol v 9 0) => [0 0]
        "maps a caret at the soft-wrap boundary to the END of the earlier row"
        (engine/caret->rowcol v 9 9) => [0 9]
        "maps the first char of the next row to its [row 0]"
        (engine/caret->rowcol v 9 10) => [1 0]
        "maps the end of the value to the last row's end"
        (engine/caret->rowcol v 9 19) => [1 9]
        "clamps an over-large caret into range"
        (engine/caret->rowcol v 9 999) => [1 9]
        "maps across a hard newline"
        (engine/caret->rowcol "a\nbb" 9 2) => [1 0]))
    (component "rowcol->caret (inverse, with clamping)"
      (assertions
        "inverts a mid-row position"
        (engine/rowcol->caret v 9 1 2) => 12
        "round-trips the end-of-line boundary caret"
        (engine/rowcol->caret v 9 0 9) => 9
        "round-trips the start of the next row"
        (engine/rowcol->caret v 9 1 0) => 10
        "clamps a too-large column to the target row's length"
        (engine/rowcol->caret v 9 0 99) => 9
        "clamps a too-large row to the last row"
        (engine/rowcol->caret v 9 99 0) => 10
        "returns 0 for the empty value"
        (engine/rowcol->caret "" 9 5 5) => 0))
    (component "round-trip property over every caret index"
      (assertions
        "rowcol->caret of caret->rowcol is the identity (at non-gap positions)"
        (every? (fn [c] (let [[r col] (engine/caret->rowcol v 9 c)]
                          (= (engine/rowcol->caret v 9 r col)
                            ;; gap positions (the dropped space at 9) collapse to end-of-line 9
                            (if (= c 9) 9 c))))
          (range 0 (inc (count v)))) => true))))

(specification {:covers {`engine/text-scroll-top "8d98bb,f91646"}} "text-scroll-top"
  (let [v "the quick brown fox jumps"]                      ; wraps at 9 to 3 rows (row 2 = "jumps")
    (assertions
      "no scroll when the caret row already fits within the window height"
      (engine/text-scroll-top v 9 0 3) => 0
      (engine/text-scroll-top v 9 25 3) => 0                ; caret row 2, window 3 -> fits from top
      "scrolls so the caret row is the last visible row when it is below the window"
      (engine/text-scroll-top v 9 25 2) => 1                ; caret row 2, height 2 -> top 1
      (engine/text-scroll-top v 9 25 1) => 2                ; caret row 2, height 1 -> top 2
      "no scroll when the caret is on the first row regardless of height"
      (engine/text-scroll-top v 9 0 1) => 0)))

(specification {:covers {`engine/multiline-input? "6f29cd,805ed0"}} "multiline-input?"
  (assertions
    "is true for an :input with :multiline? true"
    (engine/multiline-input? (elements/input {:id :n :multiline? true :value ""})) => true
    "is false for an :input without :multiline?"
    (engine/multiline-input? (elements/input {:id :n :value ""})) => false
    "is false for a non-input node even with :multiline? true"
    (engine/multiline-input? (elements/box {:multiline? true})) => false
    "is false for a non-node"
    (engine/multiline-input? "x") => false))

(specification {:covers {`engine/apply-edit-multiline "836f68,42f063"}} "apply-edit-multiline"
  (let [v "the quick brown fox"]                            ; wraps at 9 to ["the quick" "brown fox"]
    (component "Enter inserts a newline (does not submit)"
      (assertions
        "inserts \\n at the caret and advances"
        (engine/apply-edit-multiline "ab" 1 9 {:key :enter}) => {:value "a\nb" :caret 2}))

    (component "printable / backspace / delete cross line boundaries naturally"
      (assertions
        "inserts a printable char"
        (engine/apply-edit-multiline "ab" 1 9 {:key "X" :char "X"}) => {:value "aXb" :caret 2}
        "backspace at the start of a hard line joins it to the previous (removes the newline)"
        (engine/apply-edit-multiline "a\nb" 2 9 {:key :backspace}) => {:value "ab" :caret 1}
        "delete at the end of a hard line removes the following newline"
        (engine/apply-edit-multiline "a\nb" 1 9 {:key :delete}) => {:value "ab" :caret 1}))

    (component "left/right move by one char across line boundaries"
      (assertions
        "right at a soft-wrap boundary advances onto the next row's content"
        (engine/apply-edit-multiline v 9 9 {:key :right}) => {:value v :caret 10}
        "left across the boundary moves back into the previous row"
        (engine/apply-edit-multiline v 10 9 {:key :left}) => {:value v :caret 9}))

    (component "up/down move by one VISUAL line preserving the target column"
      (assertions
        "down moves to the same column on the next visual row"
        (:caret (engine/apply-edit-multiline v 2 9 {:key :down})) => 12 ; row0 col2 -> row1 col2
        "up moves to the same column on the previous visual row"
        (:caret (engine/apply-edit-multiline v 12 9 {:key :up})) => 2
        "down clamps the column to the destination row's length"
        (:caret (engine/apply-edit-multiline "abcdefghi xy" 9 9 {:key :down})) => 12
        "down on the last row leaves the caret on that row (clamped)"
        (let [[r _] (engine/caret->rowcol v 9 (:caret (engine/apply-edit-multiline v 12 9 {:key :down})))] r) => 1))

    (component "home/end move to the current VISUAL line bounds"
      (assertions
        "home moves to the start of the current visual row"
        (:caret (engine/apply-edit-multiline v 14 9 {:key :home})) => 10
        "end moves to the end of the current visual row"
        (:caret (engine/apply-edit-multiline v 12 9 {:key :end})) => 19))))

(specification {:covers {`engine/input-width      "8fceb8"
                         `engine/set-input-width! "fadec6"}} "input-width / set-input-width!"
  (let [state-atom   (atom {})
        runtime-atom (atom {})
        app          {:com.fulcrologic.fulcro.application/state-atom   state-atom
                      :com.fulcrologic.fulcro.application/runtime-atom runtime-atom}]
    (assertions
      "input-width returns the default when none is recorded"
      (engine/input-width app :notes 42) => 42)
    (engine/set-input-width! app :notes 20)
    (assertions
      "set-input-width! records the width for that input id"
      (engine/input-width app :notes 999) => 20)))

(specification {:covers {`engine/handle-input-key! "c846c3,8e9eb8"}} "handle-input-key! (single-line vs multiline)"
  (component "single-line input: Enter submits, other keys edit"
    (let [submitted (atom nil)
          changed   (atom nil)
          ra        (atom {::engine/carets {:f 1}})
          app       {:com.fulcrologic.fulcro.application/runtime-atom ra}
          node      (elements/input {:id        :f :value "abc"
                                     :on-submit (fn [v] (reset! submitted v))
                                     :on-change (fn [v c] (reset! changed [v c]))})]
      (engine/handle-input-key! app node {:key :enter})
      (assertions
        "Enter invokes :on-submit with the current value (no edit)"
        @submitted => "abc")
      (engine/handle-input-key! app node {:key "X" :char "X"})
      (assertions
        "a printable key edits via apply-edit and reports the new value/caret"
        @changed => ["aXbc" 2])))

  (component "multiline input: Enter inserts a newline (does not submit)"
    (let [submitted (atom :unset)
          changed   (atom nil)
          ra        (atom {::engine/carets {:n 1} ::engine/input-widths {:n 9}})
          app       {:com.fulcrologic.fulcro.application/runtime-atom ra}
          node      (elements/input {:id        :n :multiline? true :value "ab"
                                     :on-submit (fn [v] (reset! submitted v))
                                     :on-change (fn [v c] (reset! changed [v c]))})]
      (engine/handle-input-key! app node {:key :enter})
      (assertions
        "Enter inserts a \\n at the caret and advances it (on-change), not submitting"
        @changed => ["a\nb" 2]
        @submitted => :unset)))

  (component "multiline input: :down moves the caret by a visual line using the stored wrap width"
    (let [changed (atom nil)
          ra      (atom {::engine/carets {:n 2} ::engine/input-widths {:n 9}})
          app     {:com.fulcrologic.fulcro.application/runtime-atom ra}
          node    (elements/input {:id        :n :multiline? true :value "the quick brown fox"
                                   :on-change (fn [v c] (reset! changed [v c]))})]
      (engine/handle-input-key! app node {:key :down})
      (assertions
        ":down moves the caret to the same column one visual row down (value unchanged)"
        @changed => ["the quick brown fox" 12]
        "the new caret is written to the runtime caret store"
        (get (::engine/carets @ra) :n) => 12))))

(specification {:covers {`engine/apply-edit "73574c"}} "apply-edit"
  (component "printable insertion"
    (assertions
      "inserts a printable char at the caret and advances the caret"
      (engine/apply-edit "abc" 1 {:key "X" :char "X"}) => {:value "aXbc" :caret 2}
      "inserts at the start"
      (engine/apply-edit "abc" 0 {:key "Z" :char "Z"}) => {:value "Zabc" :caret 1}
      "inserts at the end"
      (engine/apply-edit "abc" 3 {:key "Z" :char "Z"}) => {:value "abcZ" :caret 4}
      "inserts into an empty value"
      (engine/apply-edit "" 0 {:key "q" :char "q"}) => {:value "q" :caret 1}))

  (component "backspace (delete left)"
    (assertions
      "deletes the character left of the caret and moves the caret left"
      (engine/apply-edit "abc" 2 {:key :backspace}) => {:value "ac" :caret 1}
      "is a no-op at the start of the value"
      (engine/apply-edit "abc" 0 {:key :backspace}) => {:value "abc" :caret 0}
      "is a no-op on an empty value"
      (engine/apply-edit "" 0 {:key :backspace}) => {:value "" :caret 0}))

  (component "delete (delete right)"
    (assertions
      "deletes the character right of the caret, leaving the caret in place"
      (engine/apply-edit "abc" 1 {:key :delete}) => {:value "ac" :caret 1}
      "is a no-op at the end of the value"
      (engine/apply-edit "abc" 3 {:key :delete}) => {:value "abc" :caret 3}))

  (component "caret movement"
    (assertions
      "left moves the caret one cell toward the start"
      (engine/apply-edit "abc" 2 {:key :left}) => {:value "abc" :caret 1}
      "left clamps at the start"
      (engine/apply-edit "abc" 0 {:key :left}) => {:value "abc" :caret 0}
      "right moves the caret one cell toward the end"
      (engine/apply-edit "abc" 1 {:key :right}) => {:value "abc" :caret 2}
      "right clamps at the end"
      (engine/apply-edit "abc" 3 {:key :right}) => {:value "abc" :caret 3}
      "home moves the caret to the start"
      (engine/apply-edit "abc" 2 {:key :home}) => {:value "abc" :caret 0}
      "end moves the caret to the end"
      (engine/apply-edit "abc" 0 {:key :end}) => {:value "abc" :caret 3}))

  (component "caret clamping on an out-of-range incoming caret"
    (assertions
      "clamps a too-large caret to the end before inserting"
      (engine/apply-edit "ab" 9 {:key "X" :char "X"}) => {:value "abX" :caret 3}
      "clamps a negative caret to the start before inserting"
      (engine/apply-edit "ab" -5 {:key "X" :char "X"}) => {:value "Xab" :caret 1})))

(specification {:covers {`engine/focusable-node? "4ef53d,805ed0"}} "focusable-node?"
  (assertions
    "is true for an :input with an :id"
    (engine/focusable-node? (elements/input {:id "i" :value ""})) => true
    "is true for a :button with an :id"
    (engine/focusable-node? (elements/button {:id "b"} "B")) => true
    "is true for a node explicitly marked :focusable? with an :id"
    (engine/focusable-node? (elements/text {:id "t" :focusable? true} "T")) => true
    "is FALSE for a node whose only handler is :on-key (a passive key-router, not a focus stop)"
    (engine/focusable-node? (elements/box {:id "k" :on-key (fn [_] nil)})) => false
    "is true for an :on-key node that also opts into :focusable?"
    (engine/focusable-node? (elements/box {:id "k" :focusable? true :on-key (fn [_] nil)})) => true
    "is false for a focusable-eligible node lacking an :id"
    (engine/focusable-node? (elements/button {} "B")) => false
    "is false for a plain text node"
    (engine/focusable-node? (elements/text {:id "t"} "x")) => false
    "is false for a non-node value"
    (engine/focusable-node? "not-a-node") => false))

(specification {:covers {`engine/focusables  "87a81b,bae7c4"
                         `engine/focus-order "ce7895,9b142b"
                         `engine/next-focus  "59e3a5,dbaa57"
                         `engine/prev-focus  "b15d77,dbaa57"}} "focus ring"
  (let [tree (elements/vbox {:id "root"}
               (elements/button {:id "a"} "A")
               (elements/hbox {:id "mid"}
                 (elements/input {:id "b" :value "" :priority 5})
                 (elements/button {:id "c"} "C")))
        focs (engine/focusables tree)]
    (component "focusables (document/pre-order DFS)"
      (assertions
        "collects focusable nodes in pre-order document order"
        (mapv :id focs) => ["a" "b" "c"]
        "records each node's pre-order :dfs index"
        (mapv :dfs focs) => [1 3 4]
        "reads the :priority attr (default 0)"
        (mapv :priority focs) => [0 5 0]))

    (component "focus-order (priority desc, then dfs asc)"
      (assertions
        "sorts higher priority first, ties broken by document order"
        (mapv :id (engine/focus-order focs)) => ["b" "a" "c"]))

    (let [order (engine/focus-order focs)]                  ; ["b" "a" "c"]
      (component "next-focus / prev-focus"
        (assertions
          "next-focus returns the following id in focus order"
          (engine/next-focus order "b") => "a"
          "next-focus wraps from the last to the first"
          (engine/next-focus order "c") => "b"
          "prev-focus returns the preceding id in focus order"
          (engine/prev-focus order "a") => "b"
          "prev-focus wraps from the first to the last"
          (engine/prev-focus order "b") => "c"
          "next-focus returns the first id when current is absent"
          (engine/next-focus order "missing") => "b"
          "next-focus returns the first id when current is nil"
          (engine/next-focus order nil) => "b")))))

(specification {:covers {`engine/apply-focus-change! "14481f,ecc530"}} "apply-focus-change!"
  (let [evts (atom [])
        tree (elements/vbox {:id "root"}
               (elements/button {:id "a" :on-lost-focus (fn [id] (swap! evts conj [:lost id]))} "A")
               (elements/button {:id "b" :on-focus (fn [id] (swap! evts conj [:focus id]))} "B"))]
    (component "on a real change"
      (engine/apply-focus-change! :app tree "a" "b")
      (assertions
        "fires the old node's :on-lost-focus then the new node's :on-focus, with their ids"
        @evts => [[:lost "a"] [:focus "b"]]))

    (component "when the id does not change"
      (reset! evts [])
      (engine/apply-focus-change! :app tree "a" "a")
      (assertions
        "fires no transition handlers"
        @evts => []))))

(specification {:covers {`engine/key-chord "0f3fc2"}} "key-chord"
  (assertions
    "is the :key keyword for an unmodified special key"
    (engine/key-chord {:key :tab}) => :tab
    "is the 1-char string for an unmodified printable key"
    (engine/key-chord {:key "a" :char "a"}) => "a"
    "is a [:ctrl k] vector for a ctrl-modified key"
    (engine/key-chord {:key "q" :char "q" :ctrl? true}) => [:ctrl "q"]
    "lists modifiers in :ctrl :alt :shift order before the base key"
    (engine/key-chord {:key "x" :char "x" :ctrl? true :alt? true :shift? true}) => [:ctrl :alt :shift "x"]))

(specification {:covers {`engine/route-key "98eea3,72271d"}} "route-key"
  (component "focused handler fires"
    (let [fired (atom [])
          tree  (elements/vbox {:id "root"}
                  (elements/button {:id "btn" :on-key (fn [e] (swap! fired conj (:key e)) :stop)} "B"))
          r     (engine/route-key :ctx tree "btn" {:key :enter} nil)]
      (assertions
        "the focused node's :on-key receives the event and its truthy result is returned"
        r => :stop
        "only the focused handler ran"
        @fired => [:enter])))

  (component "bubbles to an ancestor when the focused handler returns falsey"
    (let [fired (atom [])
          tree  (elements/vbox {:id "root" :on-key (fn [_] (swap! fired conj :root) :handled)}
                  (elements/hbox {:id "mid" :on-key (fn [_] (swap! fired conj :mid) nil)}
                    (elements/button {:id "btn" :on-key (fn [_] (swap! fired conj :btn) nil)} "B")))
          r     (engine/route-key :ctx tree "btn" {:key :enter} nil)]
      (assertions
        "stops at the first ancestor returning truthy, returning that result"
        r => :handled
        "ran the focused handler then bubbled through ancestors until one handled it"
        @fired => [:btn :mid :root])))

  (component "falls to the global keymap when unhandled by the node chain"
    (let [global (atom nil)
          tree   (elements/vbox {:id "root"} (elements/button {:id "btn"} "B"))
          r      (engine/route-key :ctx tree "btn" {:key "q" :char "q" :ctrl? true}
                   {[:ctrl "q"] (fn [ctx e] (reset! global [ctx (:key e)]) :quit)})]
      (assertions
        "invokes the global handler for the event's chord with context and event"
        @global => [:ctx "q"]
        "returns the global handler's truthy result"
        r => :quit)))

  (component "returns false when nothing handles the event"
    (let [tree (elements/vbox {:id "root"} (elements/button {:id "btn"} "B"))]
      (assertions
        "no node handler and no matching global chord yields false"
        (engine/route-key :ctx tree "btn" {:key :enter} {}) => false))))

;; --- process-key! driver against a synchronous raw app ---------------------

(m/defmutation set-driver-name [{:keys [v]}]
  (action [{:keys [state]}] (swap! state assoc :driver/name v)))

(comp/defsc DriverRoot [this {:keys [driver/name driver/hidden? driver/quit?]}]
  {:query         [:driver/name :driver/hidden? :driver/quit?]
   :ident         (fn [] [:component/id ::driver])
   :initial-state {:driver/name "AB" :driver/hidden? false :driver/quit? false}}
  (elements/vbox {:id "root"}
    (elements/input {:id        "name"
                     :value     name
                     :on-change (fn [v _caret] (comp/transact! this [(set-driver-name {:v v})]))})
    (when-not hidden?
      (elements/button {:id          "ok"
                        :on-focus    (fn [_] (comp/transact! this [(set-driver-name {:v "FOCUSED"})]))
                        :on-activate (fn [] (comp/transact! this [(set-driver-name {:v "ACTIVATED"})]))} "OK"))))

(defn- build-driver-app
  "Builds a synchronous raw TUI app rooted at DriverRoot, with no-op renderers so
   synchronous transactions don't drive a real terminal."
  []
  (let [app (stx/with-synchronous-transactions
              (rapp/fulcro-app {:root-class        DriverRoot
                                :core-render!      (fn [_app _opts] true)
                                :optimized-render! (fn [_app _opts] true)
                                :render-root!      (constantly true)}))]
    (rapp/initialize-state! app DriverRoot)
    app))

;; A root with a single-line input, a MULTILINE input, and a button — for arrow-key
;; focus navigation and the :multiline? capture rule.
(comp/defsc MultilineRoot [this {:keys [driver/name driver/notes]}]
  {:query         [:driver/name :driver/notes]
   :ident         (fn [] [:component/id ::ml])
   :initial-state {:driver/name "AB" :driver/notes "NOTES"}}
  (elements/vbox {:id "root"}
    (elements/input {:id        "name"
                     :value     name
                     :on-change (fn [v _caret] (comp/transact! this [(set-driver-name {:v v})]))})
    (elements/input {:id         "notes"
                     :multiline? true
                     :value      notes
                     :on-change  (fn [v _caret] (comp/transact! this [(set-driver-name {:v v})]))})
    (elements/button {:id "ok"} "OK")))

(defn- build-multiline-app
  "Builds a synchronous raw TUI app rooted at MultilineRoot."
  []
  (let [app (stx/with-synchronous-transactions
              (rapp/fulcro-app {:root-class        MultilineRoot
                                :core-render!      (fn [_app _opts] true)
                                :optimized-render! (fn [_app _opts] true)
                                :render-root!      (constantly true)}))]
    (rapp/initialize-state! app MultilineRoot)
    app))

;; A root with an input and two shortcut buttons — for control-shortcut dispatch.
(comp/defsc ShortcutRoot [this {:keys [driver/name]}]
  {:query         [:driver/name]
   :ident         (fn [] [:component/id ::sc])
   :initial-state {:driver/name "AB"}}
  (elements/vbox {:id "root"}
    (elements/input {:id        "name"
                     :value     name
                     :on-change (fn [v _caret] (comp/transact! this [(set-driver-name {:v v})]))})
    (elements/button {:id "cancel" :shortcut [:alt "c"] :shortcut-action :focus} "Cancel")
    (elements/button {:id          "save" :shortcut [:alt "s"]
                      :on-activate (fn [] (comp/transact! this [(set-driver-name {:v "SAVED"})]))} "Save")))

(defn- build-shortcut-app
  "Builds a synchronous raw TUI app rooted at ShortcutRoot."
  []
  (let [app (stx/with-synchronous-transactions
              (rapp/fulcro-app {:root-class        ShortcutRoot
                                :core-render!      (fn [_app _opts] true)
                                :optimized-render! (fn [_app _opts] true)
                                :render-root!      (constantly true)}))]
    (rapp/initialize-state! app ShortcutRoot)
    app))

(defn- alt-key
  "Returns a key-event map for Alt+`letter`."
  [letter]
  {:key letter :char nil :ctrl? false :alt? true :shift? false})

(specification {:covers {`engine/collect-shortcuts "ca9738,805ed0"}} "control shortcut dispatch (process-key!)"
  (component "an :activate shortcut (button default) focuses the target and fires :on-activate"
    (let [app (build-shortcut-app)
          sa  (:com.fulcrologic.fulcro.application/state-atom app)]
      (engine/focus! app "name")
      (binding [engine/*enhanced-keys?* true]
        (engine/process-key! app (alt-key "s")))
      (assertions
        "focus moved to the save button"
        (engine/current-focus app) => "save"
        "the button's :on-activate fired (transacted SAVED)"
        (:driver/name @sa) => "SAVED")))

  (component "a :focus shortcut-action moves focus without activating"
    (let [app (build-shortcut-app)
          sa  (:com.fulcrologic.fulcro.application/state-atom app)]
      (engine/focus! app "name")
      (binding [engine/*enhanced-keys?* true]
        (engine/process-key! app (alt-key "c")))
      (assertions
        "focus moved to the cancel button"
        (engine/current-focus app) => "cancel"
        "nothing was activated (name unchanged)"
        (:driver/name @sa) => "AB")))

  (component "a shortcut fires even while a text input is focused"
    (let [app (build-shortcut-app)
          sa  (:com.fulcrologic.fulcro.application/state-atom app)]
      (engine/focus! app "name")                            ; the input has focus
      (binding [engine/*enhanced-keys?* true]
        (engine/process-key! app (alt-key "s")))
      (assertions
        "the alt chord reaches the shortcut layer rather than being typed into the input"
        (:driver/name @sa) => "SAVED")))

  (component "shortcuts are inert when the enhanced protocol is inactive"
    (let [app (build-shortcut-app)
          sa  (:com.fulcrologic.fulcro.application/state-atom app)]
      (engine/focus! app "name")
      (engine/process-key! app (alt-key "s"))               ; *enhanced-keys?* defaults to false
      (assertions
        "focus did not move"
        (engine/current-focus app) => "name"
        "no activation happened"
        (:driver/name @sa) => "AB"))))

;; A root modelling a to-many subform: an items list (its own container, excluding the trailing
;; Add button) holding two item subforms each tagged with the same :focus-group, plus Add and Save.
;; The items container carries the Alt-j/Alt-k group-nav `:on-key` exactly as the demo wires it — a
;; passive key-router (it is NOT focusable; keys reach it by bubbling from the focused field).
(defn- fg-group-nav [app ev]
  (let [tree (engine/current-node-tree app)]
    (case (engine/key-chord ev)
      [:alt "j"] (do (engine/focus-next-in-group! app tree :line-items) :handled)
      [:alt "k"] (do (engine/focus-prev-in-group! app tree :line-items) :handled)
      nil)))

(comp/defsc FocusGroupRoot [this _props]
  {:query         [:fg/x]
   :ident         (fn [] [:component/id ::fg])
   :initial-state {:fg/x 1}}
  (elements/vbox {:id "root"}
    (elements/vbox {:id "items" :on-key (fn [ev] (fg-group-nav (comp/any->app this) ev))}
      (elements/vbox {:id "item-list"}
        (elements/vbox {:id "item-1" :focus-group :line-items}
          (elements/input {:id "i1a" :value ""})
          (elements/input {:id "i1b" :value ""}))
        (elements/vbox {:id "item-2" :focus-group :line-items}
          (elements/input {:id "i2a" :value ""})
          (elements/input {:id "i2b" :value ""})))
      (elements/button {:id "add"} "+ Add"))
    (elements/button {:id "save"} "Save")))

(defn- build-focus-group-app []
  (let [app (stx/with-synchronous-transactions
              (rapp/fulcro-app {:root-class        FocusGroupRoot
                                :core-render!      (fn [_app _opts] true)
                                :optimized-render! (fn [_app _opts] true)
                                :render-root!      (constantly true)}))]
    (rapp/initialize-state! app FocusGroupRoot)
    app))

(specification {:covers {`engine/last-focusable-in     "9fce33,c97494"
                         `engine/first-focusable-in    "b37192,ab4949"
                         `engine/focus-in!             "050509,272001"
                         `engine/focus-first-in!       "f6c47f,9768bf"
                         `engine/focus-last-in!        "b3c124,65863a"
                         `engine/focus-next-in-group!  "0c9bca,d1a9fb"
                         `engine/focus-prev-in-group!  "2b4b4d,d1a9fb"
                         `engine/focus-group-step!     "fec179,ec5aa6"}} "programmatic focus helpers"
  (component "last/first-focusable-in scope to a container's subtree"
    (let [app  (build-focus-group-app)
          tree (engine/current-node-tree app)]
      (assertions
        "last focusable in the items list is the last item's last field (the new item after an add)"
        (engine/last-focusable-in tree "item-list") => "i2b"
        "first focusable in the items list is the first item's first field"
        (engine/first-focusable-in tree "item-list") => "i1a"
        "scoping to a single item finds that item's first field"
        (engine/first-focusable-in tree "item-2") => "i2a"
        "an absent container yields nil"
        (engine/last-focusable-in tree "nope") => nil)))

  (component "focus-last-in! / focus-first-in! move focus and fire transitions"
    (let [app  (build-focus-group-app)
          tree (engine/current-node-tree app)]
      (assertions
        "focus-last-in! lands on the newest item's last field"
        (do (engine/focus-last-in! app tree "item-list") (engine/current-focus app)) => "i2b"
        "focus-first-in! lands on the first field"
        (do (engine/focus-first-in! app tree "item-list") (engine/current-focus app)) => "i1a")))

  (component "focus-next/prev-in-group! jump item-to-item, skipping inner fields, wrapping"
    (let [app  (build-focus-group-app)
          tree (engine/current-node-tree app)]
      (engine/focus! app "i1b")                             ; focus is on an inner field of item-1
      (assertions
        "next-in-group skips i1's remaining fields and lands on item-2's first field"
        (do (engine/focus-next-in-group! app tree :line-items) (engine/current-focus app)) => "i2a"
        "next-in-group from the last member wraps to the first member"
        (do (engine/focus-next-in-group! app tree :line-items) (engine/current-focus app)) => "i1a"
        "prev-in-group from the first member wraps to the last"
        (do (engine/focus-prev-in-group! app tree :line-items) (engine/current-focus app)) => "i2a")))

  (component "group step from outside the group goes to the first member"
    (let [app  (build-focus-group-app)
          tree (engine/current-node-tree app)]
      (engine/focus! app "save")                            ; not inside any group member
      (assertions
        "next-in-group lands on the first member's first field"
        (do (engine/focus-next-in-group! app tree :line-items) (engine/current-focus app)) => "i1a")))

  (component "an Alt chord bubbles past the focused INPUT to an ancestor's :on-key (group nav)"
    ;; Regression: process-key! must NOT let a focused input swallow Alt/Ctrl chords; they fall
    ;; through to route-key so the items container's group-nav :on-key fires (Alt-j/Alt-k).
    (let [app (build-focus-group-app)]
      (engine/focus! app "i1a")                             ; an inner field of item-1 is being edited
      (assertions
        "Alt-j (while the input is focused) jumps to the next group member's first field"
        (do (engine/process-key! app (alt-key "j")) (engine/current-focus app)) => "i2a"
        "Alt-k jumps back to the previous member"
        (do (engine/process-key! app (alt-key "k")) (engine/current-focus app)) => "i1a"
        "a plain letter is still typed into the focused input (not treated as navigation)"
        (do (engine/focus! app "i1a")
            (engine/process-key! app {:key "x" :char "x"})
            (engine/current-focus app)) => "i1a"))))

(specification {:covers {`engine/current-focus "8b7584"
                         `engine/focus!        "efb19d,dc1929"}} "current-focus / focus!"
  (let [app (build-driver-app)]
    (engine/focus! app "name")
    (assertions
      "focus! writes the focused id into the state-map at ::focus"
      (::engine/focus (deref (:com.fulcrologic.fulcro.application/state-atom app))) => "name"
      "current-focus reads the focused id back from the app"
      (engine/current-focus app) => "name"
      "current-focus also reads from a bare state-map"
      (engine/current-focus {::engine/focus "name"}) => "name")))

(specification {:covers {`engine/process-key! "842938,bc67eb"}} "process-key!"
  (component "Enter or Space activates a focused button"
    (let [app (build-driver-app)
          sa  (:com.fulcrologic.fulcro.application/state-atom app)]
      (assertions
        "Enter on a focused button fires its :on-activate (which transacts)"
        (do (engine/focus! app "ok") (engine/process-key! app {:key :enter}) (:driver/name @sa)) => "ACTIVATED"
        "Space on a focused button also fires :on-activate"
        (do (engine/focus! app "ok")
            (swap! sa assoc :driver/name "AB")
            (engine/process-key! app {:key " " :char " "})
            (:driver/name @sa)) => "ACTIVATED")))

  (component "Tab moves focus through the ring and wraps"
    (let [app (build-driver-app)]
      (engine/focus! app "name")
      (assertions
        "Tab advances focus to the next focusable"
        (do (engine/process-key! app {:key :tab}) (engine/current-focus app)) => "ok"
        "Tab from the last focusable wraps to the first"
        (do (engine/process-key! app {:key :tab}) (engine/current-focus app)) => "name"
        "Shift-Tab moves focus backward (wrapping)"
        (do (engine/process-key! app {:key :tab :shift? true}) (engine/current-focus app)) => "ok"
        ":backtab also moves focus backward"
        (do (engine/process-key! app {:key :backtab}) (engine/current-focus app)) => "name")))

  (component "typing into the focused input transacts the new value and advances the caret"
    (let [app (build-driver-app)
          sa  (:com.fulcrologic.fulcro.application/state-atom app)
          ra  (:com.fulcrologic.fulcro.application/runtime-atom app)]
      (engine/focus! app "name")
      (engine/process-key! app {:key "X" :char "X"})
      (assertions
        "the input's :on-change transaction updated the value in app state"
        (:driver/name @sa) => "ABX"
        "the caret store advanced to the new caret position"
        (get (::engine/carets @ra) "name") => 3)))

  (component ":on-focus fires when focus changes onto a node"
    (let [app (build-driver-app)
          sa  (:com.fulcrologic.fulcro.application/state-atom app)]
      (engine/focus! app "name")
      (engine/process-key! app {:key :tab})                 ; name -> ok, fires ok's :on-focus
      (assertions
        "the newly focused node's :on-focus handler ran (its mutation changed state)"
        (:driver/name @sa) => "FOCUSED")))

  (component "focus is re-resolved when the focused node disappears"
    (let [app (build-driver-app)
          sa  (:com.fulcrologic.fulcro.application/state-atom app)]
      ;; focus "ok", then a global-keymap handler hides it; focus should move to "name"
      (engine/focus! app "ok")
      (engine/process-key! app {:key "h" :char "h"}
        {"h" (fn [a _e] (swap! (:com.fulcrologic.fulcro.application/state-atom a)
                          assoc :driver/hidden? true))})
      (assertions
        "after the focused node is removed, focus moves to a remaining focusable"
        (engine/current-focus app) => "name")))

  (component "a global quit chord handler fires for an unfocused-by-node event"
    (let [app  (build-driver-app)
          quit (atom false)]
      (engine/focus! app "name")
      ;; ctrl-q is not consumed by the input editing path? input editing only runs for
      ;; the focused input's own keys; route-key is used when focus is not an input.
      (engine/focus! app "ok")                              ; focus the button (not an input)
      (engine/process-key! app {:key "q" :char "q" :ctrl? true}
        {[:ctrl "q"] (fn [_a _e] (reset! quit :quit))})
      (assertions
        "the global keymap handler ran for the ctrl-q chord"
        @quit => :quit)))

  (component "Down/Up arrows move focus through the ring (and wrap), like Tab/Shift-Tab"
    (let [app (build-driver-app)]
      (engine/focus! app "name")
      (assertions
        ":down advances focus to the next focusable"
        (do (engine/process-key! app {:key :down}) (engine/current-focus app)) => "ok"
        ":down from the last focusable wraps to the first"
        (do (engine/process-key! app {:key :down}) (engine/current-focus app)) => "name"
        ":up retreats focus to the previous focusable (wrapping)"
        (do (engine/process-key! app {:key :up}) (engine/current-focus app)) => "ok"
        ":up advances backward through the ring"
        (do (engine/process-key! app {:key :up}) (engine/current-focus app)) => "name")))

  (component "Down/Up on a single-line input navigate focus and leave the value unchanged"
    (let [app (build-multiline-app)
          sa  (:com.fulcrologic.fulcro.application/state-atom app)]
      (engine/focus! app "name")                            ; single-line input
      (engine/process-key! app {:key :down})
      (assertions
        "a single-line input releases :down to focus navigation (focus advances)"
        (engine/current-focus app) => "notes"
        "the single-line input's value was not edited by the navigation key"
        (:driver/name @sa) => "AB")))

  (component "a :multiline? input captures Up/Down instead of navigating focus"
    (let [app (build-multiline-app)]
      (engine/focus! app "notes")                           ; multiline input
      (engine/process-key! app {:key :down})
      (assertions
        ":down on a multiline input does not change focus (it is captured by the input)"
        (engine/current-focus app) => "notes")
      (engine/process-key! app {:key :up})
      (assertions
        ":up on a multiline input also does not change focus (it is captured)"
        (engine/current-focus app) => "notes"))))

;; ---------------------------------------------------------------------------
;; Overlays: modal node, pure helpers, layout/paint, and picker
;; ---------------------------------------------------------------------------

(specification {:covers {`engine/modal-node? "01af9e,ccafd0"}} "modal-node?"
  (assertions
    "is true for a :modal node"
    (engine/modal-node? (elements/modal {:id :d})) => true
    "is false for other nodes"
    (engine/modal-node? (elements/vbox {})) => false
    (engine/modal-node? (elements/button {:id :b} "x")) => false
    "is false for non-nodes"
    (engine/modal-node? {:a 1}) => false
    (engine/modal-node? nil) => false))

(let [tree (elements/vbox {:id "root"}
             (elements/button {:id :base} "Base")
             (elements/modal {:id :open-1 :open? true} (elements/button {:id :o1} "O1"))
             (elements/modal {:id :closed :open? false} (elements/button {:id :c} "C"))
             (elements/modal {:id :open-2 :open? true} (elements/button {:id :o2} "O2")))]

  (specification {:covers {`engine/collect-overlays "a78c8f,9d7259"}} "collect-overlays"
    (assertions
      "returns only the OPEN modal nodes (closed ones omitted)"
      (mapv #(engine/node-attr % :id) (engine/collect-overlays tree)) => [:open-1 :open-2]
      "returns them in document (pre-order) order, so the last is topmost"
      (engine/node-attr (peek (engine/collect-overlays tree)) :id) => :open-2
      "returns an empty vector when there are no open modals"
      (engine/collect-overlays (elements/vbox {} (elements/button {:id :b} "x"))) => []))

  (specification {:covers {`engine/strip-overlays "5c4c5b,a57083"}} "strip-overlays"
    (let [stripped (engine/strip-overlays tree)]
      (assertions
        "removes every :modal child (open or closed), keeping the rest"
        (mapv ::engine/tag (::engine/children stripped)) => [:button]
        "leaves no modal contents focusable in the base tree"
        (mapv :id (engine/focusables stripped)) => [:base])))

  (specification {:covers {`engine/active-tree "976472,40d11e"}} "active-tree"
    (assertions
      "returns the topmost OPEN modal as the focus-trap subtree"
      (engine/node-attr (engine/active-tree tree) :id) => :open-2
      "exposes only that modal's controls as focusable"
      (mapv :id (engine/focusables (engine/active-tree tree))) => [:o2]
      "with no open modal, returns the base tree with closed modals stripped"
      (mapv :id (engine/focusables
                  (engine/active-tree (elements/vbox {:id "r"}
                                        (elements/button {:id :base} "B")
                                        (elements/modal {:id :c :open? false}
                                          (elements/button {:id :hidden} "H"))))))
      => [:base])))

(specification {:covers {`engine/overlay-window-rect "1c450f,def90e"}} "overlay-window-rect"
  (assertions
    "centers a fixed-size window within the screen by default"
    (engine/overlay-window-rect (elements/modal {:id :d :width 10 :height 4}) {:x 0 :y 0 :w 20 :h 8})
    => {:x 5 :y 2 :w 10 :h 4}
    "resolves a fractional width/height against the screen"
    (engine/overlay-window-rect (elements/modal {:id :d :width [:fraction 0.5] :height [:fraction 0.5]})
      {:x 0 :y 0 :w 40 :h 20})
    => {:x 10 :y 5 :w 20 :h 10}
    "clamps the window to the screen size"
    (engine/overlay-window-rect (elements/modal {:id :d :width 100 :height 100}) {:x 0 :y 0 :w 20 :h 8})
    => {:x 0 :y 0 :w 20 :h 8}
    "honors :align :start (top-left)"
    (engine/overlay-window-rect (elements/modal {:id :d :width 10 :height 4 :align :start}) {:x 0 :y 0 :w 20 :h 8})
    => {:x 0 :y 0 :w 10 :h 4}))

(specification {:covers {`engine/paint "643160,702f9c"
                         `engine/place "3667ed,69acf7"}} "modal layout & paint"
  (let [m      (elements/modal {:id :d :title "Menu" :width 10 :height 4} (elements/text {} "hi"))
        placed (engine/place m {:x 2 :y 1 :w 10 :h 4})
        ;; paint a full-screen base first, then the modal on top, to prove opacity.
        base   (engine/render-buffer (engine/place (elements/text {} (apply str (repeat 14 \X))) {:x 0 :y 0 :w 14 :h 6}) 6 14)
        buf    (engine/paint base placed {:x 0 :y 0 :w 14 :h 6})
        scr    (engine/screen buf)]
    (assertions
      "places the modal at the given window rect"
      (::engine/rect placed) => {:x 2 :y 1 :w 10 :h 4}
      "draws the modal border around the window"
      (subs (nth scr 1) 2 12) => "┌─ Menu ─┐"
      "paints the modal's title onto the top border"
      (str/includes? (nth scr 1) "Menu") => true
      "renders the modal's child content inside the border"
      (subs (nth scr 2) 3 5) => "hi"
      "is opaque: base content under the window is cleared (not bleeding through)"
      (subs (nth scr 2) 3 11) => "hi      "
      "leaves base content outside the window visible"
      (subs (nth scr 0) 0 14) => "XXXXXXXXXXXXXX")))

;; ---------------------------------------------------------------------------
;; Custom rendered-tag extension (the `content-size`/`place`/`paint` multimethod seams).
;; Register namespaced `:test/*` tags so they cannot collide with the built-ins.
;; ---------------------------------------------------------------------------

(defn- test-node
  "Builds a bare engine node `{::tag ::attrs ::children}` for the extension specs."
  [tag attrs children]
  {::engine/tag tag ::engine/attrs attrs ::engine/children children})

;; A custom LEAF: natural size 3x1, paints '#' across its rect.
(defmethod engine/content-size :test/block [_] {:w 3 :h 1})
(defmethod engine/paint :test/block [buf node clip]
  (let [{:keys [x y w h]} (::engine/rect node)]
    (reduce (fn [b [cx cy]] (if (engine/in-clip? clip cx cy) (engine/put-cell b cx cy \# {}) b))
      buf (for [cy (range y (+ y h)) cx (range x (+ x w))] [cx cy]))))

;; A custom CONTAINER that reuses the built-in vbox layout via `place-stack`. A container needs BOTH
;; a `place` (lay children out) and a `paint` (recurse into the placed children).
(defmethod engine/place :test/grid [node rect]
  (let [{:keys [l r t b]} (engine/edge-insets (::engine/attrs node))
        content {:x (+ (:x rect) l) :y (+ (:y rect) t)
                 :w (max 0 (- (:w rect) l r)) :h (max 0 (- (:h rect) t b))}]
    (assoc node ::engine/rect rect
      ::engine/children (engine/place-stack :v content (mapv engine/as-node (::engine/children node))))))
(defmethod engine/paint :test/grid [buf node clip]
  (let [node-clip (engine/rect-intersection clip (::engine/rect node))]
    (reduce (fn [b child] (engine/paint b child node-clip)) buf (::engine/children node))))

(specification "custom rendered-tag extension via multimethods"
  (component "content-size dispatches to a registered custom tag"
    (assertions
      "uses the registered method's natural size"
      (engine/intrinsic-size (test-node :test/block {} [])) => {:w 3 :h 1}
      "the node's own :width still overrides the custom natural size"
      (engine/intrinsic-size (test-node :test/block {:width 5} [])) => {:w 5 :h 1}))
  (component "an unregistered custom tag falls back without error"
    (assertions
      "sizes to insets only (zero here) when nothing is declared"
      (engine/intrinsic-size (test-node :test/unknown {} [])) => {:w 0 :h 0}
      "still honors declared :width/:height on an unknown tag"
      (engine/intrinsic-size (test-node :test/unknown {:width 2 :height 4} [])) => {:w 2 :h 4}
      "paints blank (no draw) without throwing or looping"
      (engine/screen (engine/render-buffer (engine/place (test-node :test/unknown {} []) {:x 0 :y 0 :w 3 :h 1}) 1 3))
      => ["   "]))
  (component "paint dispatches to a registered custom leaf tag"
    (assertions
      "the registered method draws its own cells"
      (engine/screen (engine/render-buffer (engine/place (test-node :test/block {} []) {:x 0 :y 0 :w 3 :h 1}) 1 3))
      => ["###"]))
  (component "place dispatches to a registered custom container that reuses place-stack"
    (let [tree   (test-node :test/grid {} [(test-node :test/block {} []) (test-node :test/block {} [])])
          placed (engine/place tree {:x 0 :y 0 :w 3 :h 2})]
      (assertions
        "stacks children vertically, one row each (rects from place-stack)"
        (mapv #(::engine/rect %) (::engine/children placed))
        => [{:x 0 :y 0 :w 3 :h 1} {:x 0 :y 1 :w 3 :h 1}]
        "and the container paints both children"
        (engine/screen (engine/render-buffer placed 2 3)) => ["###" "###"]))))

;; NOTE: element-generator specs (`elements/element`, `elements/focused?`, `elements/picker`)
;; live in `com.fulcrologic.fulcro.tui.elements-spec`.
