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

(specification {:covers {`engine/node? "277a4d,ef6fa0"}} "node?"
  (assertions
    "is true for a node produced by a generator"
    (engine/node? (elements/vbox {})) => true
    "is false for a plain map without a tag"
    (engine/node? {:a 1}) => false
    "is false for a map whose tag is not legal"
    (engine/node? {::engine/tag :bogus}) => false
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

(specification {:covers {`engine/wrap-text "386083,ef97f4"}} "wrap-text"
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

(specification {:covers {`engine/wrapping-text? "e01c56,75544d"}} "wrapping-text?"
  (assertions
    "is true for a :text node with :wrap true"
    (engine/wrapping-text? (elements/text {:wrap true} "x")) => true
    "is false for a :text node without :wrap"
    (engine/wrapping-text? (elements/text {} "x")) => false
    "is false for a non-text node even with :wrap true"
    (engine/wrapping-text? (elements/box {:wrap true})) => false
    "is false for a non-node"
    (engine/wrapping-text? "x") => false))

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

(specification {:covers {`engine/intrinsic-size "df71b2,d7562d"}} "intrinsic-size"
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

(specification {:covers {`engine/place "b3e3fa,590943"}} "place"
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

  (component "cross-axis alignment"
    (assertions
      "centers a narrower child within the container width"
      (::engine/rect (first (::engine/children
                              (engine/place (elements/vbox {} (elements/text {:width 4 :align :center} "x"))
                                {:x 0 :y 0 :w 10 :h 1}))))
      => {:x 3 :y 0 :w 4 :h 1}))

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

(specification {:covers {`engine/viewport? "014fb0,75544d"}} "viewport?"
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

(specification {:covers {`engine/placed-viewports       "ade25c,96f37e"
                         `engine/focus-viewport-context "e6a13e,ca8114"}} "placed-viewports / focus-viewport-context"
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

(specification {:covers {`engine/put-str "d410fe,fc6db1"}} "put-str"
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

(specification {:covers {`engine/style->sgr-codes "6366c4,851723"}} "style->sgr-codes"
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

(specification {:covers {`engine/render-buffer "e06fec,f835b1"
                         `engine/paint         "9cb0a6,dc3f9b"}} "render-buffer / paint"
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

(specification {:covers {`engine/diff "7c8aea,3667e7"}} "diff"
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

(specification {:covers {`engine/ops->ansi "856e75,5e36b9"}} "ops->ansi"
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

(specification {:covers {`engine/frame->ansi "9eafc2,a5ab29"}} "frame->ansi"
  (let [b1  (engine/make-buffer 1 5)
        nxt (engine/put-str b1 0 0 "X" {} {:x 0 :y 0 :w 5 :h 1})]
    (assertions
      "wraps the diff in DEC private mode 2026 when :sync? is true"
      (engine/frame->ansi b1 nxt {:sync? true}) => "[?2026h[1;1HX[?2026l"
      "does not wrap the diff when :sync? is false"
      (engine/frame->ansi b1 nxt {:sync? false}) => "[1;1HX")))

(specification {:covers {`engine/frame->ansi "9eafc2,a5ab29"}} "frame->ansi — clear-screen on resize"
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

(specification {:covers {`engine/render-tree "2ad3f6,408f57"}} "render-tree"
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

(specification {:covers {`engine/render-root "b79137,64eef5"}} "render-root"
  (let [tree (engine/render-root Plain {:p/id 5 :p/label "Root"})]
    (assertions
      "builds the root instance via factory and walks it to a pure node tree"
      tree => (elements/button {:id "plain-5"} "Root"))))

(specification {:covers {`engine/node-attr "d53a76,75544d"}} "node-attr"
  (assertions
    "returns the value of an attribute on a node"
    (engine/node-attr (elements/text {:id "x" :color :red} "t") :color) => :red
    "returns nil for a missing attribute"
    (engine/node-attr (elements/text {} "t") :id) => nil
    "returns nil for a non-node value"
    (engine/node-attr "not-a-node" :id) => nil))

(specification {:covers {`engine/find-by-id "3fafe1,8ae121"}} "find-by-id"
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

(specification {:covers {`engine/node-text "752bdf,75544d"}} "node-text"
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

(specification {:covers {`engine/activate! "99f954,8ae121"}} "activate!"
  (let [called (atom false)
        node   (elements/button {:id "go" :on-activate (fn [] (reset! called :yes))} "Go")]
    (assertions
      "invokes the node's :on-activate handler"
      (engine/activate! node) => :yes
      "the handler was actually run"
      (deref called) => :yes
      "returns nil when there is no handler"
      (engine/activate! (elements/button {} "x")) => nil)))

(specification {:covers {`engine/press! "f6f187,8ae121"}} "press!"
  (let [seen (atom nil)
        node (elements/box {:id "b" :on-key (fn [k] (reset! seen k))})]
    (assertions
      "invokes the node's :on-key handler with the key"
      (engine/press! node :enter) => :enter
      "the handler received the key"
      (deref seen) => :enter
      "returns nil when there is no handler"
      (engine/press! (elements/box {}) :x) => nil)))

(specification {:covers {`engine/type! "e574a3,8ae121"}} "type!"
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

(specification {:covers {`engine/wrap-layout "f43751,6d2e82"}} "wrap-layout"
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

(specification {:covers {`engine/caret->rowcol "17bca2,ee2367"
                         `engine/rowcol->caret "2b265f,ee2367"}} "caret <-> rowcol"
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

(specification {:covers {`engine/text-scroll-top "8d98bb,01b510"}} "text-scroll-top"
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

(specification {:covers {`engine/multiline-input? "6f29cd,8ae121"}} "multiline-input?"
  (assertions
    "is true for an :input with :multiline? true"
    (engine/multiline-input? (elements/input {:id :n :multiline? true :value ""})) => true
    "is false for an :input without :multiline?"
    (engine/multiline-input? (elements/input {:id :n :value ""})) => false
    "is false for a non-input node even with :multiline? true"
    (engine/multiline-input? (elements/box {:multiline? true})) => false
    "is false for a non-node"
    (engine/multiline-input? "x") => false))

(specification {:covers {`engine/apply-edit-multiline "836f68,1a11ff"}} "apply-edit-multiline"
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

(specification {:covers {`engine/handle-input-key! "11ab98,ac6054"}} "handle-input-key! (single-line vs multiline)"
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

(specification {:covers {`engine/focusable-node? "578cb2,8ae121"}} "focusable-node?"
  (assertions
    "is true for an :input with an :id"
    (engine/focusable-node? (elements/input {:id "i" :value ""})) => true
    "is true for a :button with an :id"
    (engine/focusable-node? (elements/button {:id "b"} "B")) => true
    "is true for a node explicitly marked :focusable? with an :id"
    (engine/focusable-node? (elements/text {:id "t" :focusable? true} "T")) => true
    "is true for a node carrying an :on-key handler with an :id"
    (engine/focusable-node? (elements/box {:id "k" :on-key (fn [_] nil)})) => true
    "is false for a focusable-eligible node lacking an :id"
    (engine/focusable-node? (elements/button {} "B")) => false
    "is false for a plain text node"
    (engine/focusable-node? (elements/text {:id "t"} "x")) => false
    "is false for a non-node value"
    (engine/focusable-node? "not-a-node") => false))

(specification {:covers {`engine/focusables  "af1f15,f9541e"
                         `engine/focus-order "ce7895,60de49"
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

(specification {:covers {`engine/apply-focus-change! "14481f,804804"}} "apply-focus-change!"
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

(specification {:covers {`engine/route-key "d7b983,806419"}} "route-key"
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
                     :on-change (fn [v _caret] (elements/transact! this [(set-driver-name {:v v})]))})
    (when-not hidden?
      (elements/button {:id          "ok"
                        :on-focus    (fn [_] (elements/transact! this [(set-driver-name {:v "FOCUSED"})]))
                        :on-activate (fn [] (elements/transact! this [(set-driver-name {:v "ACTIVATED"})]))} "OK"))))

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
                     :on-change (fn [v _caret] (elements/transact! this [(set-driver-name {:v v})]))})
    (elements/input {:id         "notes"
                     :multiline? true
                     :value      notes
                     :on-change  (fn [v _caret] (elements/transact! this [(set-driver-name {:v v})]))})
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

(specification {:covers {`engine/current-focus "8b7584"
                         `engine/focus!        "f79a95"}} "current-focus / focus!"
  (let [app (build-driver-app)]
    (engine/focus! app "name")
    (assertions
      "focus! writes the focused id into the state-map at ::focus"
      (::engine/focus (deref (:com.fulcrologic.fulcro.application/state-atom app))) => "name"
      "current-focus reads the focused id back from the app"
      (engine/current-focus app) => "name"
      "current-focus also reads from a bare state-map"
      (engine/current-focus {::engine/focus "name"}) => "name")))

(specification {:covers {`engine/process-key! "a34332,a99726"}} "process-key!"
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

(specification {:covers {`engine/modal-node? "01af9e,75544d"}} "modal-node?"
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

  (specification {:covers {`engine/collect-overlays "a78c8f,2a7273"}} "collect-overlays"
    (assertions
      "returns only the OPEN modal nodes (closed ones omitted)"
      (mapv #(engine/node-attr % :id) (engine/collect-overlays tree)) => [:open-1 :open-2]
      "returns them in document (pre-order) order, so the last is topmost"
      (engine/node-attr (peek (engine/collect-overlays tree)) :id) => :open-2
      "returns an empty vector when there are no open modals"
      (engine/collect-overlays (elements/vbox {} (elements/button {:id :b} "x"))) => []))

  (specification {:covers {`engine/strip-overlays "5c4c5b,c25b8c"}} "strip-overlays"
    (let [stripped (engine/strip-overlays tree)]
      (assertions
        "removes every :modal child (open or closed), keeping the rest"
        (mapv ::engine/tag (::engine/children stripped)) => [:button]
        "leaves no modal contents focusable in the base tree"
        (mapv :id (engine/focusables stripped)) => [:base])))

  (specification {:covers {`engine/active-tree "976472,93d1d8"}} "active-tree"
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

(specification {:covers {`engine/overlay-window-rect "1c450f,2e15d4"}} "overlay-window-rect"
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

(specification {:covers {`engine/paint "9cb0a6,dc3f9b"
                         `engine/place "b3e3fa,590943"}} "modal layout & paint"
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

;; NOTE: element-generator specs (`elements/element`, `elements/focused?`, `elements/picker`)
;; live in `com.fulcrologic.fulcro.tui.elements-spec`.
