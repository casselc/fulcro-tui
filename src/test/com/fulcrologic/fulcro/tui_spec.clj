(ns com.fulcrologic.fulcro.tui-spec
  (:require
    [com.fulcrologic.fulcro.algorithms.tx-processing.synchronous-tx-processing :as stx]
    [com.fulcrologic.fulcro.mutations :as m]
    [com.fulcrologic.fulcro.raw.application :as rapp]
    [com.fulcrologic.fulcro.raw.components :as rc]
    [com.fulcrologic.fulcro.tui :as tui]
    [fulcro-spec.core :refer [specification component assertions => =throws=>]]))

;; ---------------------------------------------------------------------------
;; Component fixtures for the walker / utility specs (defined via tui/defsc)
;; ---------------------------------------------------------------------------

(tui/defsc Leaf [this {:keys [leaf/id leaf/label]} {:keys [on-select]}]
  {:query [:leaf/id :leaf/label]
   :ident :leaf/id}
  (tui/button {:id (str "leaf-" id) :on-activate on-select} label))

(def ui-leaf (tui/computed-factory Leaf))

(tui/defsc Container [this {:keys [c/title c/leaf]}]
  {:query [:c/title {:c/leaf (rc/get-query Leaf)}]
   :ident :c/title}
  (tui/vbox {:id "root"}
    (tui/text {:id "title"} title)
    (ui-leaf leaf {:on-select (fn [] :selected)})))

(tui/defsc Plain [this {:keys [p/id p/label]}]
  {:query [:p/id :p/label]
   :ident :p/id}
  (tui/button {:id (str "plain-" id)} label))

(tui/defsc Multi [this {:keys [m/id]}]
  {:query [:m/id]
   :ident :m/id}
  [(tui/text {} "a") (tui/text {} "b")])

(specification {:covers {`tui/node? "277a4d,a1f917"}} "node?"
  (assertions
    "is true for a node produced by a generator"
    (tui/node? (tui/vbox {})) => true
    "is false for a plain map without a tag"
    (tui/node? {:a 1}) => false
    "is false for a map whose tag is not legal"
    (tui/node? {::tui/tag :bogus}) => false
    "is false for non-map values"
    (tui/node? "x") => false
    (tui/node? nil) => false))

(specification {:covers {`tui/element "0b6694,1b7634"}} "element generators"
  (component "attribute handling"
    (assertions
      "uses a leading map as the node's attributes"
      (::tui/attrs (tui/vbox {:width 3} "x")) => {:width 3}
      "defaults attributes to an empty map when the first arg is not a map"
      (::tui/attrs (tui/vbox "x")) => {}))

  (component "children"
    (assertions
      "keeps node and string children in order"
      (::tui/children (tui/vbox {} "a" (tui/text {} "b"))) => ["a" (tui/text {} "b")]
      "flattens nested sequences spliced into the children"
      (::tui/children (tui/vbox {} (list "a" "b") "c")) => ["a" "b" "c"]
      "removes nil children"
      (::tui/children (tui/vbox {} "a" nil "b")) => ["a" "b"]))

  (component "tags"
    (assertions
      "each generator stamps its own tag"
      (::tui/tag (tui/vbox)) => :vbox
      (::tui/tag (tui/hbox)) => :hbox
      (::tui/tag (tui/box)) => :box
      (::tui/tag (tui/text)) => :text
      (::tui/tag (tui/button)) => :button
      (::tui/tag (tui/line)) => :line
      (::tui/tag (tui/viewport)) => :viewport))

  (component "input"
    (assertions
      "is a childless leaf carrying only its attributes"
      (tui/input {:id :x :value "hi"})
      => {::tui/tag :input ::tui/attrs {:id :x :value "hi"} ::tui/children []})))

(specification {:covers {`tui/code-point-width "bb5a0d,d044cd"}} "code-point-width"
  (assertions
    "is 1 for an ordinary ASCII letter"
    (tui/code-point-width (int \A)) => 1
    "is 0 for control characters (newline)"
    (tui/code-point-width (int \newline)) => 0
    "is 0 for a combining mark"
    (tui/code-point-width 0x0301) => 0
    "is 2 for a CJK ideograph"
    (tui/code-point-width 0x4E00) => 2
    "is 2 for a fullwidth form"
    (tui/code-point-width 0xFF21) => 2
    "is 2 for an emoji"
    (tui/code-point-width 0x1F600) => 2))

(specification {:covers {`tui/string-width "6aa7c8,398b8d"}} "string-width"
  (assertions
    "counts one column per ASCII character"
    (tui/string-width "hello") => 5
    "counts wide characters as two columns"
    (tui/string-width "ab一") => 4
    "ignores zero-width combining marks (precomposed or decomposed)"
    (tui/string-width "áb") => 2
    "is zero for the empty string"
    (tui/string-width "") => 0))

(specification {:covers {`tui/wrap-text "386083,30a362"}} "wrap-text"
  (component "short text fits on one line"
    (assertions
      "text narrower than the width is a single line"
      (tui/wrap-text "hello" 10) => ["hello"]
      "text exactly the width is a single line"
      (tui/wrap-text "hello" 5) => ["hello"]))

  (component "greedy word wrapping"
    (assertions
      "packs words onto a line until the next word would overflow, then breaks"
      (tui/wrap-text "the quick brown fox" 9) => ["the quick" "brown fox"]
      "puts each word on its own line when only one fits"
      (tui/wrap-text "aaa bbb ccc" 3) => ["aaa" "bbb" "ccc"]))

  (component "over-long words"
    (assertions
      "hard-breaks a single word wider than the width into width-sized pieces"
      (tui/wrap-text "abcdefgh" 3) => ["abc" "def" "gh"]
      "breaks the long head then continues wrapping the remaining words"
      (tui/wrap-text "supercalifragilistic and more" 10)
      => ["supercalif" "ragilistic" "and more"]))

  (component "embedded newlines"
    (assertions
      "splits on hard newlines before word-wrapping each segment"
      (tui/wrap-text "line1\nline2" 10) => ["line1" "line2"]
      "a blank segment between newlines yields an empty visual line"
      (tui/wrap-text "a\n\nb" 10) => ["a" "" "b"]))

  (component "spaces and empties"
    (assertions
      "preserves runs of spaces that fit within a line"
      (tui/wrap-text "a  b c" 10) => ["a  b c"]
      "the empty string is one empty line"
      (tui/wrap-text "" 5) => [""]))

  (component "wide characters measured by display width"
    (assertions
      "hard-breaks wide (2-column) characters without splitting one across the boundary"
      (tui/wrap-text "一二三 abc" 4) => ["一二" "三" "abc"]))

  (component "non-positive width disables wrapping"
    (assertions
      "width 0 returns each newline segment as one line"
      (tui/wrap-text "x y z" 0) => ["x y z"]
      (tui/wrap-text "a b\nc d" 0) => ["a b" "c d"])))

(specification {:covers {`tui/wrapping-text? "e01c56,860769"}} "wrapping-text?"
  (assertions
    "is true for a :text node with :wrap true"
    (tui/wrapping-text? (tui/text {:wrap true} "x")) => true
    "is false for a :text node without :wrap"
    (tui/wrapping-text? (tui/text {} "x")) => false
    "is false for a non-text node even with :wrap true"
    (tui/wrapping-text? (tui/box {:wrap true})) => false
    "is false for a non-node"
    (tui/wrapping-text? "x") => false))

(specification "wrapping text layout (height-for-width)"
  (component "place in a vbox: height is the wrapped line count at the container width"
    (let [placed (tui/place (tui/vbox {} (tui/text {:wrap true} "the quick brown fox jumps"))
                   {:x 0 :y 0 :w 9 :h 10})
          child  (first (::tui/children placed))]
      (assertions
        "the wrapping text fills the container width on the cross axis"
        (:w (::tui/rect child)) => 9
        "its height equals the number of wrapped lines at that width"
        (:h (::tui/rect child)) => 3
        (:h (::tui/rect child)) => (count (tui/wrap-text "the quick brown fox jumps" 9)))))

  (component "render-buffer paints the wrapped lines into the rect (clipped)"
    (let [placed (tui/place (tui/vbox {} (tui/text {:wrap true} "the quick brown fox jumps"))
                   {:x 0 :y 0 :w 9 :h 10})
          buf    (tui/render-buffer placed 4 9)]
      (assertions
        "successive wrapped lines render on successive rows"
        (take 3 (tui/screen buf)) => ["the quick" "brown fox" "jumps    "])))

  (component "a long wrapped paragraph inside a viewport wraps and scrolls"
    (let [para   "the quick brown fox jumps over the lazy dog"
          placed (tui/place (tui/viewport {:id :vp :height 3} (tui/text {:wrap true} para))
                   {:x 0 :y 0 :w 9 :h 3})]
      (assertions
        "the viewport's virtual height is the wrapped line count at its content width"
        (:h (::tui/virtual-size placed)) => (count (tui/wrap-text para 9))
        "with scroll 0 the first wrapped lines are shown"
        (tui/screen (tui/render-buffer (assoc placed ::tui/scroll {:x 0 :y 0}) 3 9))
        => ["the quick" "brown fox" "jumps    "]
        "scrolling down reveals deeper wrapped lines"
        (tui/screen (tui/render-buffer (assoc placed ::tui/scroll {:x 0 :y 2}) 3 9))
        => ["jumps    " "over the " "lazy dog "]))))

(specification {:covers {`tui/intrinsic-size "df71b2,744213"}} "intrinsic-size"
  (component "leaves"
    (assertions
      "text is as wide as its longest line and as tall as its line count"
      (tui/intrinsic-size (tui/text {} "ab\ncde")) => {:w 3 :h 2}
      "single-line text is one row tall"
      (tui/intrinsic-size (tui/text {} "hi")) => {:w 2 :h 1}
      "a button is as wide as its label, one row tall"
      (tui/intrinsic-size (tui/button {} "OK")) => {:w 2 :h 1}
      "an input is as wide as its value"
      (tui/intrinsic-size (tui/input {:value "abc"})) => {:w 3 :h 1}
      "an empty input is at least one column wide"
      (tui/intrinsic-size (tui/input {})) => {:w 1 :h 1}
      "a viewport takes its declared fixed size"
      (tui/intrinsic-size (tui/viewport {:width 20 :height 5})) => {:w 20 :h 5}))

  (component "containers"
    (assertions
      "a vbox is as wide as its widest child and as tall as the sum of child heights"
      (tui/intrinsic-size (tui/vbox {} (tui/text {} "ab") (tui/text {} "cde"))) => {:w 3 :h 2}
      "an hbox is as wide as the sum of child widths and as tall as its tallest child"
      (tui/intrinsic-size (tui/hbox {} (tui/text {} "ab") (tui/text {} "cde"))) => {:w 5 :h 1}))

  (component "insets and overrides"
    (assertions
      "a border adds one cell on every edge"
      (tui/intrinsic-size (tui/box {:border? true} (tui/text {} "hi"))) => {:w 4 :h 3}
      "padding adds its value on every edge"
      (tui/intrinsic-size (tui/box {:padding 2} (tui/text {} "hi"))) => {:w 6 :h 5}
      "a fixed :width overrides the content width"
      (tui/intrinsic-size (tui/text {:width 8} "hi")) => {:w 8 :h 1}
      "min-width floors the width (winning over a smaller fixed width)"
      (tui/intrinsic-size (tui/text {:width 8 :min-width 10} "hi")) => {:w 10 :h 1})))

(defn- child-rects
  "Returns the vector of child ::rect maps of a placed node."
  [placed]
  (mapv ::tui/rect (::tui/children placed)))

(specification {:covers {`tui/place "c2da82,d826ec"}} "place"
  (component "the node's own rect"
    (assertions
      "is the outer rect it was placed in"
      (::tui/rect (tui/place (tui/vbox {}) {:x 2 :y 3 :w 8 :h 4})) => {:x 2 :y 3 :w 8 :h 4}))

  (component "vbox stacking (vertical)"
    (assertions
      "places fixed-height children sequentially, filling width on the cross axis"
      (child-rects (tui/place (tui/vbox {} (tui/text {:height 1} "a") (tui/text {:height 2} "b"))
                     {:x 0 :y 0 :w 10 :h 4}))
      => [{:x 0 :y 0 :w 10 :h 1} {:x 0 :y 1 :w 10 :h 2}]
      "gives a :grow child the height left over after a fixed child"
      (child-rects (tui/place (tui/vbox {} (tui/text {:height 1} "h") (tui/text {:grow 1} "body"))
                     {:x 0 :y 0 :w 6 :h 5}))
      => [{:x 0 :y 0 :w 6 :h 1} {:x 0 :y 1 :w 6 :h 4}]
      "splits leftover height evenly between two equal-weight grow children"
      (mapv :h (child-rects (tui/place (tui/vbox {} (tui/text {:grow 1} "a") (tui/text {:grow 1} "b"))
                              {:x 0 :y 0 :w 4 :h 4})))
      => [2 2]))

  (component "hbox stacking (horizontal)"
    (assertions
      "gives a fixed-width sidebar its width and the rest to a grow child"
      (mapv :w (child-rects (tui/place (tui/hbox {} (tui/box {:width 3}) (tui/box {:grow 1}))
                              {:x 0 :y 0 :w 10 :h 2})))
      => [3 7]
      "splits width between two :half children"
      (mapv :w (child-rects (tui/place (tui/hbox {} (tui/box {:width :half}) (tui/box {:width :half}))
                              {:x 0 :y 0 :w 10 :h 2})))
      => [5 5]))

  (component "cross-axis alignment"
    (assertions
      "centers a narrower child within the container width"
      (::tui/rect (first (::tui/children
                           (tui/place (tui/vbox {} (tui/text {:width 4 :align :center} "x"))
                             {:x 0 :y 0 :w 10 :h 1}))))
      => {:x 3 :y 0 :w 4 :h 1}))

  (component "insets"
    (assertions
      "a border insets the content area by one cell on each edge"
      (child-rects (tui/place (tui/vbox {:border? true} (tui/text {} "x"))
                     {:x 0 :y 0 :w 5 :h 3}))
      => [{:x 1 :y 1 :w 3 :h 1}]))

  (component "viewport"
    (let [tall   (tui/vbox {} (tui/text {} "L0") (tui/text {} "L1") (tui/text {} "L2")
                   (tui/text {} "L3") (tui/text {} "L4"))
          placed (tui/place (tui/viewport {:id :vp :height 3} tall) {:x 0 :y 0 :w 4 :h 3})]
      (assertions
        "lays its single child out at the child's NATURAL height (taller than the viewport)"
        (::tui/rect (::tui/viewport-content placed)) => {:x 0 :y 0 :w 4 :h 5}
        "records the virtual content size (content width x natural height)"
        (::tui/virtual-size placed) => {:w 4 :h 5}
        "places the virtual child in 0-based virtual coordinates, not absolute screen coords"
        (mapv ::tui/rect (::tui/children (::tui/viewport-content placed)))
        => [{:x 0 :y 0 :w 4 :h 1} {:x 0 :y 1 :w 4 :h 1} {:x 0 :y 2 :w 4 :h 1}
            {:x 0 :y 3 :w 4 :h 1} {:x 0 :y 4 :w 4 :h 1}]
        "attaches a default scroll offset of {0,0}"
        (::tui/scroll placed) => {:x 0 :y 0}
        "the viewport's own outer rect is the rect it was placed in"
        (::tui/rect placed) => {:x 0 :y 0 :w 4 :h 3}))))

(specification {:covers {`tui/clamp-scroll "cd8b2d"}} "clamp-scroll"
  (assertions
    "clamps a too-small (negative) offset up to 0 on both axes"
    (tui/clamp-scroll {:x -3 :y -2} {:w 10 :h 10} {:w 4 :h 4}) => {:x 0 :y 0}
    "clamps a too-large offset down to (virtual - view) on both axes"
    (tui/clamp-scroll {:x 99 :y 99} {:w 10 :h 10} {:w 4 :h 4}) => {:x 6 :y 6}
    "leaves an in-range offset unchanged"
    (tui/clamp-scroll {:x 2 :y 3} {:w 10 :h 10} {:w 4 :h 4}) => {:x 2 :y 3}
    "clamps to zero on an axis whose virtual size is no larger than the view"
    (tui/clamp-scroll {:x 5 :y 5} {:w 4 :h 4} {:w 4 :h 4}) => {:x 0 :y 0}
    (tui/clamp-scroll {:x 5 :y 5} {:w 3 :h 3} {:w 4 :h 4}) => {:x 0 :y 0}))

(specification {:covers {`tui/scroll-to-show "8c2a68"}} "scroll-to-show"
  (assertions
    "scrolls forward minimally so a below-window rect's far edge is the last visible cell"
    (tui/scroll-to-show {:x 0 :y 0} {:x 0 :y 4 :w 4 :h 1} {:w 4 :h 3}) => {:x 0 :y 2}
    "scrolls back to a rect's near edge when it is above the window"
    (tui/scroll-to-show {:x 0 :y 2} {:x 0 :y 0 :w 4 :h 1} {:w 4 :h 3}) => {:x 0 :y 0}
    "leaves scroll unchanged when the rect is already fully visible"
    (tui/scroll-to-show {:x 0 :y 2} {:x 0 :y 3 :w 4 :h 1} {:w 4 :h 3}) => {:x 0 :y 2}
    "shows the near edge of a rect taller than the window"
    (tui/scroll-to-show {:x 0 :y 0} {:x 0 :y 1 :w 4 :h 5} {:w 4 :h 3}) => {:x 0 :y 1}
    "adjusts both axes independently"
    (tui/scroll-to-show {:x 0 :y 0} {:x 5 :y 4 :w 1 :h 1} {:w 3 :h 3}) => {:x 3 :y 2}))

(specification {:covers {`tui/viewport? "014fb0,860769"}} "viewport?"
  (assertions
    "is true for a viewport node"
    (tui/viewport? (tui/viewport {})) => true
    "is false for other node tags"
    (tui/viewport? (tui/box {})) => false
    "is false for non-nodes"
    (tui/viewport? "x") => false))

(specification {:covers {`tui/content-view-size "9380cf"}} "content-view-size"
  (assertions
    "is the rect size when there are no insets"
    (tui/content-view-size (tui/place (tui/viewport {}) {:x 0 :y 0 :w 8 :h 4})) => {:w 8 :h 4}
    "shrinks by one cell per edge for a border"
    (tui/content-view-size (tui/place (tui/viewport {:border? true}) {:x 0 :y 0 :w 8 :h 4})) => {:w 6 :h 2}
    "shrinks by the padding on every edge"
    (tui/content-view-size (tui/place (tui/viewport {:padding 1}) {:x 0 :y 0 :w 8 :h 4})) => {:w 6 :h 2}))

(specification {:covers {`tui/placed-viewports "ade25c,41162a"
                         `tui/focus-viewport-context "e6a13e,16d92a"}} "placed-viewports / focus-viewport-context"
  (let [tree   (tui/vbox {:id "root"}
                 (tui/text {} "hdr")
                 (tui/viewport {:id :vp}
                   (tui/vbox {} (tui/button {:id :i0} "I0") (tui/button {:id :i1} "I1")
                     (tui/button {:id :i2} "I2") (tui/button {:id :i3} "I3"))))
        placed (tui/place tree {:x 0 :y 0 :w 6 :h 3})]
    (component "placed-viewports"
      (assertions
        "finds the viewport node in the placed tree"
        (mapv #(tui/node-attr % :id) (tui/placed-viewports placed)) => [:vp]))
    (component "focus-viewport-context"
      (assertions
        "identifies the enclosing viewport of a focused node"
        (tui/node-attr (:viewport (tui/focus-viewport-context placed :i2)) :id) => :vp
        "returns the focused node's VIRTUAL rect (0-based within the viewport)"
        (:virtual-rect (tui/focus-viewport-context placed :i2)) => {:x 0 :y 2 :w 6 :h 1}
        (:virtual-rect (tui/focus-viewport-context placed :i0)) => {:x 0 :y 0 :w 6 :h 1}
        "returns nil for an id that is not inside any viewport"
        (tui/focus-viewport-context placed :nope) => nil))))

(specification {:covers {`tui/viewport-scroll "c571ab,797b46"
                         `tui/all-scroll "68fe31"
                         `tui/set-viewport-scroll! "60901d"}} "viewport scroll state"
  (let [state-atom (atom {})
        app        {:com.fulcrologic.fulcro.application/state-atom state-atom}]
    (assertions
      "viewport-scroll defaults to {0,0} when none is recorded"
      (tui/viewport-scroll app :vp) => {:x 0 :y 0}
      "all-scroll is empty when none is recorded"
      (tui/all-scroll app) => {})
    (tui/set-viewport-scroll! app :vp {:x 1 :y 4})
    (assertions
      "set-viewport-scroll! records the offset for that viewport id"
      (tui/viewport-scroll app :vp) => {:x 1 :y 4}
      "all-scroll returns the whole {id {:x :y}} map"
      (tui/all-scroll app) => {:vp {:x 1 :y 4}}
      "viewport-scroll reads from a raw state-map too"
      (tui/viewport-scroll (deref state-atom) :vp) => {:x 1 :y 4})))

;; ============================================================================
;; Render
;; ============================================================================

(specification {:covers {`tui/make-buffer "d01cec,26290c"}} "make-buffer"
  (let [b (tui/make-buffer 2 3)]
    (assertions
      "has the requested row count"
      (:rows b) => 2
      "has the requested column count"
      (:cols b) => 3
      "holds rows*cols cells row-major"
      (count (:cells b)) => 6
      "fills every cell with a space in the default (empty) style"
      (set (:cells b)) => #{{:ch \space :sgr {}}})))

(specification {:covers {`tui/put-cell "990a89,71d211"}} "put-cell"
  (let [b (tui/make-buffer 2 3)]
    (assertions
      "sets the character and style at the given 0-based (x,y)"
      (tui/screen (tui/put-cell b 1 0 \X {:fg :red})) => [" X " "   "]
      "stores the style on the targeted cell"
      (:sgr (get-in (tui/screen-styled (tui/put-cell b 1 0 \X {:fg :red})) [0 1])) => {:fg :red}
      "indexes y as the row and x as the column"
      (tui/screen (tui/put-cell b 0 1 \Z {})) => ["   " "Z  "]
      "ignores writes outside the buffer bounds (clipped), returning the buffer unchanged"
      (tui/put-cell b 9 9 \X {}) => b
      (tui/put-cell b -1 0 \X {}) => b)))

(specification {:covers {`tui/put-str "d410fe,fc6db1"}} "put-str"
  (let [b    (tui/make-buffer 1 6)
        full {:x 0 :y 0 :w 6 :h 1}]
    (component "advancing and styling"
      (assertions
        "writes a string starting at (x,y)"
        (tui/screen (tui/put-str b 2 0 "hi" {} full)) => ["  hi  "]
        "styles every written cell"
        (mapv :sgr (subvec (first (tui/screen-styled (tui/put-str b 0 0 "ab" {:fg :red} full))) 0 2))
        => [{:fg :red} {:fg :red}]
        "advances a wide (2-column) character by two columns"
        (tui/screen (tui/put-str b 0 0 "一b" {} full)) => ["一b   "]))

    (component "clipping"
      (assertions
        "truncates text that runs past the clip rect width"
        (tui/screen (tui/put-str b 0 0 "hello" {} {:x 0 :y 0 :w 2 :h 1})) => ["he    "]
        "truncates text that runs past the buffer bounds"
        (tui/screen (tui/put-str b 4 0 "hello" {} full)) => ["    he"]
        "does not write a wide char whose second cell falls outside the clip"
        (tui/screen (tui/put-str b 0 0 "一" {} {:x 0 :y 0 :w 1 :h 1})) => ["      "]))))

(specification {:covers {`tui/style->sgr-codes "6366c4,851723"}} "style->sgr-codes"
  (assertions
    "returns an empty vector for the default style"
    (tui/style->sgr-codes {}) => []
    "maps :color/:fg to its palette code"
    (tui/style->sgr-codes {:fg :red}) => [31]
    "maps a bright color to its 90s code"
    (tui/style->sgr-codes {:fg :bright-white}) => [97]
    "maps :bg to the foreground code plus 10"
    (tui/style->sgr-codes {:bg :blue}) => [44]
    "emits reverse (7) and bold (1) before the colors, in that order"
    (tui/style->sgr-codes {:reverse? true :bold? true :fg :red :bg :blue}) => [7 1 31 44]))

(specification {:covers {`tui/sgr-string "0edb71"}} "sgr-string"
  (assertions
    "joins codes with semicolons inside a CSI ...m sequence"
    (tui/sgr-string [1 31]) => "[1;31m"
    "emits a single code with no separators"
    (tui/sgr-string [44]) => "[44m"
    "yields the reset sequence for an empty codes vector"
    (tui/sgr-string []) => "[0m"))

(specification {:covers {`tui/render-buffer "e06fec,305801"
                         `tui/paint        "9cb0a6,a3dafe"}} "render-buffer / paint"
  (component "stacked leaves"
    (let [tree (tui/place (tui/vbox {}
                            (tui/text {} "Hello")
                            (tui/button {:highlight true} "OK")
                            (tui/line {}))
                 {:x 0 :y 0 :w 6 :h 3})
          buf  (tui/render-buffer tree 3 6)]
      (assertions
        "paints text, a button label, and a horizontal rule on successive rows"
        (tui/screen buf) => ["Hello " "OK    " "──────"]
        "renders a :highlight button cell with reverse video"
        (:sgr (get-in (tui/screen-styled buf) [1 0])) => {:reverse? true})))

  (component "color and bold styling"
    (let [buf (tui/render-buffer
                (tui/place (tui/text {:color :red :bold true} "X") {:x 0 :y 0 :w 3 :h 1}) 1 3)]
      (assertions
        "applies :color as a foreground and :bold as bold to text cells"
        (:sgr (get-in (tui/screen-styled buf) [0 0])) => {:fg :red :bold? true})))

  (component "input value"
    (let [buf (tui/render-buffer (tui/place (tui/input {:value "hi"}) {:x 0 :y 0 :w 4 :h 1}) 1 4)]
      (assertions
        "writes the input's :value into its rect"
        (tui/screen buf) => ["hi  "])))

  (component "vertical line"
    (let [buf (tui/render-buffer (tui/place (tui/line {}) {:x 0 :y 0 :w 1 :h 3}) 3 1)]
      (assertions
        "fills a taller-than-wide line rect with a vertical rule"
        (tui/screen buf) => ["│" "│" "│"])))

  (component "border drawing"
    (let [buf (tui/render-buffer (tui/place (tui/box {:border? true} (tui/text {} "x"))
                                   {:x 0 :y 0 :w 5 :h 3}) 3 5)]
      (assertions
        "draws box-drawing corners and edges around the outer rect with the child inside"
        (tui/screen buf) => ["┌───┐" "│x  │" "└───┘"])))

  (component "background fill"
    (let [buf (tui/render-buffer (tui/place (tui/box {:bg :blue :width 2 :height 1})
                                   {:x 0 :y 0 :w 2 :h 1}) 1 2)]
      (assertions
        "fills the content rect with the background style when :bg is set"
        (mapv :sgr (first (tui/screen-styled buf))) => [{:bg :blue} {:bg :blue}])))

  (component "child clipping to parent"
    (let [buf (tui/render-buffer
                (tui/place (tui/box {:width 3 :height 1} (tui/text {} "ABCDEFG"))
                  {:x 0 :y 0 :w 3 :h 1}) 1 6)]
      (assertions
        "a child cannot paint outside its parent's rect"
        (tui/screen buf) => ["ABC   "])))

  (component "viewport scrolling"
    (let [tall   (tui/vbox {} (tui/text {} "AA") (tui/text {} "BB") (tui/text {} "CC")
                   (tui/text {} "DD") (tui/text {} "EE"))
          placed (tui/place (tui/viewport {:id :vp :height 3} tall) {:x 0 :y 0 :w 2 :h 3})]
      (assertions
        "with scroll {:y 0} the top rows of the (taller) child are shown"
        (tui/screen (tui/render-buffer (assoc placed ::tui/scroll {:x 0 :y 0}) 3 2)) => ["AA" "BB" "CC"]
        "with scroll {:y k} the window starting at virtual row k is shown"
        (tui/screen (tui/render-buffer (assoc placed ::tui/scroll {:x 0 :y 2}) 3 2)) => ["CC" "DD" "EE"]
        "an out-of-range scroll is clamped so the last window is shown"
        (tui/screen (tui/render-buffer (assoc placed ::tui/scroll {:x 0 :y 99}) 3 2)) => ["CC" "DD" "EE"])))

  (component "viewport content is clipped to the viewport rect"
    (let [tall   (tui/vbox {} (tui/text {} "WIDE0") (tui/text {} "WIDE1") (tui/text {} "WIDE2")
                   (tui/text {} "WIDE3") (tui/text {} "WIDE4"))
          ;; viewport content area is 3 wide x 2 tall; place it offset into a larger buffer
          placed (tui/place (tui/viewport {:id :vp :width 3 :height 2} tall) {:x 1 :y 1 :w 3 :h 2})
          buf    (tui/render-buffer placed 4 6)]
      (assertions
        "rows above/below and columns left/right of the viewport stay blank (content cannot leak out)"
        (tui/screen buf) => ["      "
                             " WID  "
                             " WID  "
                             "      "])))

  (component "viewport border is drawn on the outer rect"
    (let [tall   (tui/vbox {} (tui/text {} "rr") (tui/text {} "ss") (tui/text {} "tt") (tui/text {} "uu"))
          placed (tui/place (tui/viewport {:id :vp :border? true} tall) {:x 0 :y 0 :w 4 :h 4})
          buf    (tui/render-buffer placed 4 4)]
      (assertions
        "draws box-drawing corners/edges around the viewport with the scrolled content inside"
        (tui/screen buf) => ["┌──┐" "│rr│" "│ss│" "└──┘"]
        "scrolling shows a deeper window inside the same border"
        (tui/screen (tui/render-buffer (assoc placed ::tui/scroll {:x 0 :y 2}) 4 4))
        => ["┌──┐" "│tt│" "│uu│" "└──┘"]))))

(specification {:covers {`tui/screen "f01462,64763b"}} "screen"
  (let [b (tui/make-buffer 2 3)]
    (assertions
      "returns one joined string per row"
      (tui/screen (tui/put-cell (tui/put-cell b 0 0 \a {}) 0 1 \b {})) => ["a  " "b  "]
      "drops the continuation cell that follows a wide character"
      (tui/screen (tui/put-str b 0 0 "一" {} {:x 0 :y 0 :w 3 :h 2})) => ["一 " "   "]
      "returns a blank string of spaces for an untouched row"
      (tui/screen b) => ["   " "   "])))

(specification {:covers {`tui/screen-styled "6390e9"}} "screen-styled"
  (let [b   (tui/make-buffer 1 2)
        out (tui/screen-styled (tui/put-cell b 0 0 \a {:fg :red}))]
    (assertions
      "returns one vector of cells per row"
      (count out) => 1
      "exposes the character of each cell"
      (mapv :ch (first out)) => [\a \space]
      "exposes the style of each cell"
      (mapv :sgr (first out)) => [{:fg :red} {}])))

(specification {:covers {`tui/diff "7c8aea,3667e7"}} "diff"
  (let [b1 (tui/make-buffer 1 5)
        ab (tui/put-str b1 0 0 "abc" {} {:x 0 :y 0 :w 5 :h 1})]
    (component "unchanged buffers"
      (assertions
        "produces no ops when the buffers are identical"
        (tui/diff ab ab) => []))

    (component "coalescing a run"
      (let [red-abc (tui/put-str b1 0 0 "abc" {:fg :red} {:x 0 :y 0 :w 5 :h 1})
            red-XYZ (tui/put-str b1 0 0 "XYZ" {:fg :red} {:x 0 :y 0 :w 5 :h 1})]
        (assertions
          "emits a single op for a run of adjacent changed cells that share a style"
          (tui/diff red-abc red-XYZ) => [{:row 0 :col 0 :sgr [31] :text "XYZ"}])))

    (component "non-adjacent changes"
      (let [xbz (tui/put-str b1 0 0 "xbz" {} {:x 0 :y 0 :w 5 :h 1})]
        (assertions
          "emits a separate op for each run separated by unchanged cells"
          (tui/diff ab xbz)
          => [{:row 0 :col 0 :sgr [] :text "x"} {:row 0 :col 2 :sgr [] :text "z"}])))

    (component "full repaint"
      (assertions
        "emits a full coalesced repaint of non-default cells when prev is nil"
        (tui/diff nil ab) => [{:row 0 :col 0 :sgr [] :text "abc"}]
        "emits a full repaint when prev has different dimensions"
        (tui/diff (tui/make-buffer 2 5) ab) => [{:row 0 :col 0 :sgr [] :text "abc"}]))))

(specification {:covers {`tui/ops->ansi "4c4047,2ef542"}} "ops->ansi"
  (assertions
    "emits a cursor move (row+1;col+1) then an SGR then the text"
    (tui/ops->ansi [{:row 1 :col 2 :sgr [31] :text "hi"}]) => "[2;3H[31mhi[0m"
    "suppresses a redundant SGR when the pen style is unchanged between ops"
    (tui/ops->ansi [{:row 0 :col 0 :sgr [31] :text "a"} {:row 0 :col 3 :sgr [31] :text "b"}])
    => "[1;1H[31ma[1;4Hb[0m"
    "emits a new SGR when the pen style changes between ops"
    (tui/ops->ansi [{:row 0 :col 0 :sgr [31] :text "a"} {:row 0 :col 3 :sgr [] :text "b"}])
    => "[1;1H[31ma[1;4H[0mb"
    "returns the empty string for no ops"
    (tui/ops->ansi []) => ""))

(specification {:covers {`tui/frame->ansi "960851,ddde12"}} "frame->ansi"
  (let [b1  (tui/make-buffer 1 5)
        nxt (tui/put-str b1 0 0 "X" {} {:x 0 :y 0 :w 5 :h 1})]
    (assertions
      "wraps the diff in DEC private mode 2026 when :sync? is true"
      (tui/frame->ansi b1 nxt {:sync? true}) => "[?2026h[1;1HX[?2026l"
      "does not wrap the diff when :sync? is false"
      (tui/frame->ansi b1 nxt {:sync? false}) => "[1;1HX")))


;; ===========================================================================
;; Components & walker
;; ===========================================================================

(specification "tui/defsc compile-time checks"
  (assertions
    "throws when a prop is destructured but is not in the :query"
    (tui/tui-defsc* {} '(Bad [this {:keys [a/x a/y]}]
                          {:query [:a/x] :ident :a/x} nil))
    =throws=> clojure.lang.ExceptionInfo
    "throws when the :ident key is not in the :query"
    (tui/tui-defsc* {} '(Bad2 [this {:keys [a/x]}]
                          {:query [:a/x] :ident :a/z} nil))
    =throws=> clojure.lang.ExceptionInfo
    "expands a valid form to a def of a faux class"
    (let [expansion (tui/tui-defsc* {} '(Ok [this {:keys [a/x]}]
                                          {:query [:a/x] :ident :a/x} nil))]
      (first expansion)) => 'def))

(specification {:covers {`tui/factory "cbae71,0b521c"}} "factory"
  (let [f        (tui/factory Leaf)
        instance (f {:leaf/id 7 :leaf/label "L"})]
    (assertions
      "returns a function that builds a component instance"
      (rc/component-instance? instance) => true
      "stamps the class onto the instance"
      (:fulcro$class instance) => Leaf
      "carries the given props under :fulcro$value so rc/props reads them"
      (rc/props instance) => {:leaf/id 7 :leaf/label "L"}
      "binds *app* (default nil) into the instance props"
      (-> instance :props :fulcro$app) => nil
      "the instance supports rc/get-ident"
      (rc/get-ident instance) => [:leaf/id 7]
      "records a :fulcro$reactKey when props carry a :react-key"
      (-> (f {:leaf/id 7 :react-key "k"}) :props :fulcro$reactKey) => "k"
      "omits :fulcro$reactKey when there is no :react-key"
      (-> (f {:leaf/id 7}) :props (contains? :fulcro$reactKey)) => false))
  (component "with a bound *app*"
    (binding [tui/*app* :the-app]
      (assertions
        "stamps the bound *app* onto the instance"
        (-> ((tui/factory Leaf) {:leaf/id 1}) :props :fulcro$app) => :the-app))))

(specification {:covers {`tui/computed-factory "3426d7,879f5d"}} "computed-factory"
  (let [f        (tui/computed-factory Leaf)
        instance (f {:leaf/id 1 :leaf/label "L"} {:on-select :CB})]
    (assertions
      "attaches the computed map so rc/get-computed retrieves it"
      (rc/get-computed instance) => {:on-select :CB}
      "still exposes the underlying props"
      (select-keys (rc/props instance) [:leaf/id :leaf/label]) => {:leaf/id 1 :leaf/label "L"}
      "works with no computed map"
      (rc/get-computed (f {:leaf/id 1})) => nil)))

(specification {:covers {`tui/render-instance "fa649a"}} "render-instance"
  (let [instance ((tui/factory Plain) {:p/id 3 :p/label "Click"})]
    (assertions
      "calls the class :render, returning its node with props flowing in"
      (tui/render-instance instance) => (tui/button {:id "plain-3"} "Click"))))

(specification {:covers {`tui/render-tree "4667e2,690bb1"}} "render-tree"
  (component "scalars and nil"
    (assertions
      "passes a string through unchanged"
      (tui/render-tree "hi") => "hi"
      "passes a number through unchanged"
      (tui/render-tree 42) => 42
      "drops nil"
      (tui/render-tree nil) => nil))
  (component "nodes"
    (assertions
      "keeps a leaf node's tag, attrs, and text"
      (tui/render-tree (tui/text {:id "t"} "x")) => (tui/text {:id "t"} "x")))
  (component "component instances"
    (let [tree (tui/render-root Container
                 {:c/title "Hello"
                  :c/leaf  {:leaf/id 1 :leaf/label "Press"}})]
      (assertions
        "produces a node tree for place"
        (tui/node? tree) => true
        "leaves no component instances among the children"
        (some rc/component-instance? (::tui/children tree)) => nil
        "splices a nested child component's rendered node into the parent's children"
        (-> tree ::tui/children second ::tui/tag) => :button
        (-> tree ::tui/children second ::tui/children) => ["Press"]
        "renders the parent's own text node"
        (-> tree ::tui/children first ::tui/children) => ["Hello"])))
  (component "vector render splices siblings"
    (let [tree (tui/render-tree ((tui/factory Multi) {:m/id 1}))]
      (assertions
        "a render returning a vector yields a vector of sibling nodes"
        tree => [(tui/text {} "a") (tui/text {} "b")]))))

(specification {:covers {`tui/render-root "156754,5efd76"}} "render-root"
  (let [tree (tui/render-root Plain {:p/id 5 :p/label "Root"})]
    (assertions
      "builds the root instance via factory and walks it to a pure node tree"
      tree => (tui/button {:id "plain-5"} "Root"))))

(specification {:covers {`tui/node-attr "d53a76,860769"}} "node-attr"
  (assertions
    "returns the value of an attribute on a node"
    (tui/node-attr (tui/text {:id "x" :color :red} "t") :color) => :red
    "returns nil for a missing attribute"
    (tui/node-attr (tui/text {} "t") :id) => nil
    "returns nil for a non-node value"
    (tui/node-attr "not-a-node" :id) => nil))

(specification {:covers {`tui/find-by-id "3fafe1,d74f47"}} "find-by-id"
  (let [tree (tui/vbox {:id "root"}
               (tui/text {:id "a"} "A")
               (tui/hbox {:id "mid"}
                 (tui/button {:id "target"} "hit")))]
    (assertions
      "finds a deeply nested node by its :id"
      (tui/find-by-id tree "target") => (tui/button {:id "target"} "hit")
      "returns the node itself when it matches"
      (tui/find-by-id tree "root") => tree
      "returns nil when no node has the id"
      (tui/find-by-id tree "nope") => nil)))

(specification {:covers {`tui/node-text "752bdf,860769"}} "node-text"
  (let [tree (tui/vbox {}
               (tui/text {} "Hello ")
               (tui/hbox {} (tui/text {} "wor") "ld"))]
    (assertions
      "concatenates the text of a node and all its descendants"
      (tui/node-text tree) => "Hello world"
      "returns a bare string unchanged"
      (tui/node-text "abc") => "abc"
      "stringifies a bare number"
      (tui/node-text 42) => "42")))

(specification {:covers {`tui/activate! "99f954,d74f47"}} "activate!"
  (let [called (atom false)
        node   (tui/button {:id "go" :on-activate (fn [] (reset! called :yes))} "Go")]
    (assertions
      "invokes the node's :on-activate handler"
      (tui/activate! node) => :yes
      "the handler was actually run"
      (deref called) => :yes
      "returns nil when there is no handler"
      (tui/activate! (tui/button {} "x")) => nil)))

(specification {:covers {`tui/press! "f6f187,d74f47"}} "press!"
  (let [seen (atom nil)
        node (tui/box {:id "b" :on-key (fn [k] (reset! seen k))})]
    (assertions
      "invokes the node's :on-key handler with the key"
      (tui/press! node :enter) => :enter
      "the handler received the key"
      (deref seen) => :enter
      "returns nil when there is no handler"
      (tui/press! (tui/box {}) :x) => nil)))

(specification {:covers {`tui/type! "e574a3,d74f47"}} "type!"
  (let [typed (atom nil)
        node  (tui/input {:id "f" :value "" :on-change (fn [s] (reset! typed s))})]
    (assertions
      "invokes the node's :on-change handler with the proposed string"
      (tui/type! node "abc") => "abc"
      "the handler received the proposed string"
      (deref typed) => "abc"
      "returns nil when there is no handler"
      (tui/type! (tui/input {:value ""}) "z") => nil)))

;; ===========================================================================
;; Focus, input & key dispatch
;; ===========================================================================

(specification {:covers {`tui/wrap-layout "f43751,a6cfe3"}} "wrap-layout"
  (assertions
    "records each wrapped row's text, start caret index, and consumed length"
    (tui/wrap-layout "the quick brown fox" 9)
    => [{:start 0 :len 9 :text "the quick"} {:start 10 :len 9 :text "brown fox"}]
    "advances :start across the dropped soft-wrap space (caret 9 is the gap)"
    (mapv :start (tui/wrap-layout "the quick brown fox" 9)) => [0 10]
    "advances :start across a hard newline"
    (tui/wrap-layout "a\nbb\nccc" 9)
    => [{:start 0 :len 1 :text "a"} {:start 2 :len 2 :text "bb"} {:start 5 :len 3 :text "ccc"}]
    "yields a single empty row for the empty string"
    (tui/wrap-layout "" 9) => [{:start 0 :len 0 :text ""}]
    "represents a blank line between newlines as an empty row at the right caret index"
    (tui/wrap-layout "a\n\nb" 9) => [{:start 0 :len 1 :text "a"} {:start 2 :len 0 :text ""} {:start 3 :len 1 :text "b"}]
    "puts a trailing newline's empty line after the content"
    (tui/wrap-layout "ab\n" 9) => [{:start 0 :len 2 :text "ab"} {:start 3 :len 0 :text ""}]))

(specification {:covers {`tui/caret->rowcol "17bca2,957be0"
                         `tui/rowcol->caret "2b265f,957be0"}} "caret <-> rowcol"
  (let [v "the quick brown fox"]                            ; wraps at 9 to ["the quick" "brown fox"]
    (component "caret->rowcol"
      (assertions
        "maps the start of the value to [0 0]"
        (tui/caret->rowcol v 9 0) => [0 0]
        "maps a caret at the soft-wrap boundary to the END of the earlier row"
        (tui/caret->rowcol v 9 9) => [0 9]
        "maps the first char of the next row to its [row 0]"
        (tui/caret->rowcol v 9 10) => [1 0]
        "maps the end of the value to the last row's end"
        (tui/caret->rowcol v 9 19) => [1 9]
        "clamps an over-large caret into range"
        (tui/caret->rowcol v 9 999) => [1 9]
        "maps across a hard newline"
        (tui/caret->rowcol "a\nbb" 9 2) => [1 0]))
    (component "rowcol->caret (inverse, with clamping)"
      (assertions
        "inverts a mid-row position"
        (tui/rowcol->caret v 9 1 2) => 12
        "round-trips the end-of-line boundary caret"
        (tui/rowcol->caret v 9 0 9) => 9
        "round-trips the start of the next row"
        (tui/rowcol->caret v 9 1 0) => 10
        "clamps a too-large column to the target row's length"
        (tui/rowcol->caret v 9 0 99) => 9
        "clamps a too-large row to the last row"
        (tui/rowcol->caret v 9 99 0) => 10
        "returns 0 for the empty value"
        (tui/rowcol->caret "" 9 5 5) => 0))
    (component "round-trip property over every caret index"
      (assertions
        "rowcol->caret of caret->rowcol is the identity (at non-gap positions)"
        (every? (fn [c] (let [[r col] (tui/caret->rowcol v 9 c)]
                          (= (tui/rowcol->caret v 9 r col)
                            ;; gap positions (the dropped space at 9) collapse to end-of-line 9
                            (if (= c 9) 9 c))))
          (range 0 (inc (count v)))) => true))))

(specification {:covers {`tui/text-scroll-top "8d98bb,9b3c3f"}} "text-scroll-top"
  (let [v "the quick brown fox jumps"]                      ; wraps at 9 to 3 rows (row 2 = "jumps")
    (assertions
      "no scroll when the caret row already fits within the window height"
      (tui/text-scroll-top v 9 0 3) => 0
      (tui/text-scroll-top v 9 25 3) => 0                   ; caret row 2, window 3 -> fits from top
      "scrolls so the caret row is the last visible row when it is below the window"
      (tui/text-scroll-top v 9 25 2) => 1                   ; caret row 2, height 2 -> top 1
      (tui/text-scroll-top v 9 25 1) => 2                   ; caret row 2, height 1 -> top 2
      "no scroll when the caret is on the first row regardless of height"
      (tui/text-scroll-top v 9 0 1) => 0)))

(specification {:covers {`tui/multiline-input? "6f29cd,d74f47"}} "multiline-input?"
  (assertions
    "is true for an :input with :multiline? true"
    (tui/multiline-input? (tui/input {:id :n :multiline? true :value ""})) => true
    "is false for an :input without :multiline?"
    (tui/multiline-input? (tui/input {:id :n :value ""})) => false
    "is false for a non-input node even with :multiline? true"
    (tui/multiline-input? (tui/box {:multiline? true})) => false
    "is false for a non-node"
    (tui/multiline-input? "x") => false))

(specification {:covers {`tui/apply-edit-multiline "836f68,575f84"}} "apply-edit-multiline"
  (let [v "the quick brown fox"]                            ; wraps at 9 to ["the quick" "brown fox"]
    (component "Enter inserts a newline (does not submit)"
      (assertions
        "inserts \\n at the caret and advances"
        (tui/apply-edit-multiline "ab" 1 9 {:key :enter}) => {:value "a\nb" :caret 2}))

    (component "printable / backspace / delete cross line boundaries naturally"
      (assertions
        "inserts a printable char"
        (tui/apply-edit-multiline "ab" 1 9 {:key "X" :char "X"}) => {:value "aXb" :caret 2}
        "backspace at the start of a hard line joins it to the previous (removes the newline)"
        (tui/apply-edit-multiline "a\nb" 2 9 {:key :backspace}) => {:value "ab" :caret 1}
        "delete at the end of a hard line removes the following newline"
        (tui/apply-edit-multiline "a\nb" 1 9 {:key :delete}) => {:value "ab" :caret 1}))

    (component "left/right move by one char across line boundaries"
      (assertions
        "right at a soft-wrap boundary advances onto the next row's content"
        (tui/apply-edit-multiline v 9 9 {:key :right}) => {:value v :caret 10}
        "left across the boundary moves back into the previous row"
        (tui/apply-edit-multiline v 10 9 {:key :left}) => {:value v :caret 9}))

    (component "up/down move by one VISUAL line preserving the target column"
      (assertions
        "down moves to the same column on the next visual row"
        (:caret (tui/apply-edit-multiline v 2 9 {:key :down})) => 12         ; row0 col2 -> row1 col2
        "up moves to the same column on the previous visual row"
        (:caret (tui/apply-edit-multiline v 12 9 {:key :up})) => 2
        "down clamps the column to the destination row's length"
        (:caret (tui/apply-edit-multiline "abcdefghi xy" 9 9 {:key :down})) => 12
        "down on the last row leaves the caret on that row (clamped)"
        (let [[r _] (tui/caret->rowcol v 9 (:caret (tui/apply-edit-multiline v 12 9 {:key :down})))] r) => 1))

    (component "home/end move to the current VISUAL line bounds"
      (assertions
        "home moves to the start of the current visual row"
        (:caret (tui/apply-edit-multiline v 14 9 {:key :home})) => 10
        "end moves to the end of the current visual row"
        (:caret (tui/apply-edit-multiline v 12 9 {:key :end})) => 19))))

(specification {:covers {`tui/input-width "8fceb8"
                         `tui/set-input-width! "fadec6"}} "input-width / set-input-width!"
  (let [state-atom   (atom {})
        runtime-atom (atom {})
        app          {:com.fulcrologic.fulcro.application/state-atom state-atom
                      :com.fulcrologic.fulcro.application/runtime-atom runtime-atom}]
    (assertions
      "input-width returns the default when none is recorded"
      (tui/input-width app :notes 42) => 42)
    (tui/set-input-width! app :notes 20)
    (assertions
      "set-input-width! records the width for that input id"
      (tui/input-width app :notes 999) => 20)))

(specification {:covers {`tui/handle-input-key! "11ab98,f3bd17"}} "handle-input-key! (single-line vs multiline)"
  (component "single-line input: Enter submits, other keys edit"
    (let [submitted (atom nil)
          changed   (atom nil)
          ra        (atom {::tui/carets {:f 1}})
          app       {:com.fulcrologic.fulcro.application/runtime-atom ra}
          node      (tui/input {:id :f :value "abc"
                                :on-submit (fn [v] (reset! submitted v))
                                :on-change (fn [v c] (reset! changed [v c]))})]
      (tui/handle-input-key! app node {:key :enter})
      (assertions
        "Enter invokes :on-submit with the current value (no edit)"
        @submitted => "abc")
      (tui/handle-input-key! app node {:key "X" :char "X"})
      (assertions
        "a printable key edits via apply-edit and reports the new value/caret"
        @changed => ["aXbc" 2])))

  (component "multiline input: Enter inserts a newline (does not submit)"
    (let [submitted (atom :unset)
          changed   (atom nil)
          ra        (atom {::tui/carets {:n 1} ::tui/input-widths {:n 9}})
          app       {:com.fulcrologic.fulcro.application/runtime-atom ra}
          node      (tui/input {:id :n :multiline? true :value "ab"
                                :on-submit (fn [v] (reset! submitted v))
                                :on-change (fn [v c] (reset! changed [v c]))})]
      (tui/handle-input-key! app node {:key :enter})
      (assertions
        "Enter inserts a \\n at the caret and advances it (on-change), not submitting"
        @changed => ["a\nb" 2]
        @submitted => :unset)))

  (component "multiline input: :down moves the caret by a visual line using the stored wrap width"
    (let [changed (atom nil)
          ra      (atom {::tui/carets {:n 2} ::tui/input-widths {:n 9}})
          app     {:com.fulcrologic.fulcro.application/runtime-atom ra}
          node    (tui/input {:id :n :multiline? true :value "the quick brown fox"
                              :on-change (fn [v c] (reset! changed [v c]))})]
      (tui/handle-input-key! app node {:key :down})
      (assertions
        ":down moves the caret to the same column one visual row down (value unchanged)"
        @changed => ["the quick brown fox" 12]
        "the new caret is written to the runtime caret store"
        (get (::tui/carets @ra) :n) => 12))))

(specification {:covers {`tui/apply-edit "73574c"}} "apply-edit"
  (component "printable insertion"
    (assertions
      "inserts a printable char at the caret and advances the caret"
      (tui/apply-edit "abc" 1 {:key "X" :char "X"}) => {:value "aXbc" :caret 2}
      "inserts at the start"
      (tui/apply-edit "abc" 0 {:key "Z" :char "Z"}) => {:value "Zabc" :caret 1}
      "inserts at the end"
      (tui/apply-edit "abc" 3 {:key "Z" :char "Z"}) => {:value "abcZ" :caret 4}
      "inserts into an empty value"
      (tui/apply-edit "" 0 {:key "q" :char "q"}) => {:value "q" :caret 1}))

  (component "backspace (delete left)"
    (assertions
      "deletes the character left of the caret and moves the caret left"
      (tui/apply-edit "abc" 2 {:key :backspace}) => {:value "ac" :caret 1}
      "is a no-op at the start of the value"
      (tui/apply-edit "abc" 0 {:key :backspace}) => {:value "abc" :caret 0}
      "is a no-op on an empty value"
      (tui/apply-edit "" 0 {:key :backspace}) => {:value "" :caret 0}))

  (component "delete (delete right)"
    (assertions
      "deletes the character right of the caret, leaving the caret in place"
      (tui/apply-edit "abc" 1 {:key :delete}) => {:value "ac" :caret 1}
      "is a no-op at the end of the value"
      (tui/apply-edit "abc" 3 {:key :delete}) => {:value "abc" :caret 3}))

  (component "caret movement"
    (assertions
      "left moves the caret one cell toward the start"
      (tui/apply-edit "abc" 2 {:key :left}) => {:value "abc" :caret 1}
      "left clamps at the start"
      (tui/apply-edit "abc" 0 {:key :left}) => {:value "abc" :caret 0}
      "right moves the caret one cell toward the end"
      (tui/apply-edit "abc" 1 {:key :right}) => {:value "abc" :caret 2}
      "right clamps at the end"
      (tui/apply-edit "abc" 3 {:key :right}) => {:value "abc" :caret 3}
      "home moves the caret to the start"
      (tui/apply-edit "abc" 2 {:key :home}) => {:value "abc" :caret 0}
      "end moves the caret to the end"
      (tui/apply-edit "abc" 0 {:key :end}) => {:value "abc" :caret 3}))

  (component "caret clamping on an out-of-range incoming caret"
    (assertions
      "clamps a too-large caret to the end before inserting"
      (tui/apply-edit "ab" 9 {:key "X" :char "X"}) => {:value "abX" :caret 3}
      "clamps a negative caret to the start before inserting"
      (tui/apply-edit "ab" -5 {:key "X" :char "X"}) => {:value "Xab" :caret 1})))

(specification {:covers {`tui/focusable-node? "578cb2,d74f47"}} "focusable-node?"
  (assertions
    "is true for an :input with an :id"
    (tui/focusable-node? (tui/input {:id "i" :value ""})) => true
    "is true for a :button with an :id"
    (tui/focusable-node? (tui/button {:id "b"} "B")) => true
    "is true for a node explicitly marked :focusable? with an :id"
    (tui/focusable-node? (tui/text {:id "t" :focusable? true} "T")) => true
    "is true for a node carrying an :on-key handler with an :id"
    (tui/focusable-node? (tui/box {:id "k" :on-key (fn [_] nil)})) => true
    "is false for a focusable-eligible node lacking an :id"
    (tui/focusable-node? (tui/button {} "B")) => false
    "is false for a plain text node"
    (tui/focusable-node? (tui/text {:id "t"} "x")) => false
    "is false for a non-node value"
    (tui/focusable-node? "not-a-node") => false))

(specification {:covers {`tui/focusables       "af1f15,775ae0"
                         `tui/focus-order      "ce7895,defd92"
                         `tui/next-focus       "59e3a5,dbaa57"
                         `tui/prev-focus       "b15d77,dbaa57"}} "focus ring"
  (let [tree (tui/vbox {:id "root"}
               (tui/button {:id "a"} "A")
               (tui/hbox {:id "mid"}
                 (tui/input {:id "b" :value "" :priority 5})
                 (tui/button {:id "c"} "C")))
        focs (tui/focusables tree)]
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
        (mapv :id (tui/focus-order focs)) => ["b" "a" "c"]))

    (let [order (tui/focus-order focs)]   ; ["b" "a" "c"]
      (component "next-focus / prev-focus"
        (assertions
          "next-focus returns the following id in focus order"
          (tui/next-focus order "b") => "a"
          "next-focus wraps from the last to the first"
          (tui/next-focus order "c") => "b"
          "prev-focus returns the preceding id in focus order"
          (tui/prev-focus order "a") => "b"
          "prev-focus wraps from the first to the last"
          (tui/prev-focus order "b") => "c"
          "next-focus returns the first id when current is absent"
          (tui/next-focus order "missing") => "b"
          "next-focus returns the first id when current is nil"
          (tui/next-focus order nil) => "b")))))

(specification {:covers {`tui/apply-focus-change! "14481f,a22c50"}} "apply-focus-change!"
  (let [evts (atom [])
        tree (tui/vbox {:id "root"}
               (tui/button {:id "a" :on-lost-focus (fn [id] (swap! evts conj [:lost id]))} "A")
               (tui/button {:id "b" :on-focus (fn [id] (swap! evts conj [:focus id]))} "B"))]
    (component "on a real change"
      (tui/apply-focus-change! :app tree "a" "b")
      (assertions
        "fires the old node's :on-lost-focus then the new node's :on-focus, with their ids"
        @evts => [[:lost "a"] [:focus "b"]]))

    (component "when the id does not change"
      (reset! evts [])
      (tui/apply-focus-change! :app tree "a" "a")
      (assertions
        "fires no transition handlers"
        @evts => []))))

(specification {:covers {`tui/key-chord "0f3fc2"}} "key-chord"
  (assertions
    "is the :key keyword for an unmodified special key"
    (tui/key-chord {:key :tab}) => :tab
    "is the 1-char string for an unmodified printable key"
    (tui/key-chord {:key "a" :char "a"}) => "a"
    "is a [:ctrl k] vector for a ctrl-modified key"
    (tui/key-chord {:key "q" :char "q" :ctrl? true}) => [:ctrl "q"]
    "lists modifiers in :ctrl :alt :shift order before the base key"
    (tui/key-chord {:key "x" :char "x" :ctrl? true :alt? true :shift? true}) => [:ctrl :alt :shift "x"]))

(specification {:covers {`tui/route-key "d7b983,b2bea2"}} "route-key"
  (component "focused handler fires"
    (let [fired (atom [])
          tree  (tui/vbox {:id "root"}
                  (tui/button {:id "btn" :on-key (fn [e] (swap! fired conj (:key e)) :stop)} "B"))
          r     (tui/route-key :ctx tree "btn" {:key :enter} nil)]
      (assertions
        "the focused node's :on-key receives the event and its truthy result is returned"
        r => :stop
        "only the focused handler ran"
        @fired => [:enter])))

  (component "bubbles to an ancestor when the focused handler returns falsey"
    (let [fired (atom [])
          tree  (tui/vbox {:id "root" :on-key (fn [_] (swap! fired conj :root) :handled)}
                  (tui/hbox {:id "mid" :on-key (fn [_] (swap! fired conj :mid) nil)}
                    (tui/button {:id "btn" :on-key (fn [_] (swap! fired conj :btn) nil)} "B")))
          r     (tui/route-key :ctx tree "btn" {:key :enter} nil)]
      (assertions
        "stops at the first ancestor returning truthy, returning that result"
        r => :handled
        "ran the focused handler then bubbled through ancestors until one handled it"
        @fired => [:btn :mid :root])))

  (component "falls to the global keymap when unhandled by the node chain"
    (let [global (atom nil)
          tree   (tui/vbox {:id "root"} (tui/button {:id "btn"} "B"))
          r      (tui/route-key :ctx tree "btn" {:key "q" :char "q" :ctrl? true}
                   {[:ctrl "q"] (fn [ctx e] (reset! global [ctx (:key e)]) :quit)})]
      (assertions
        "invokes the global handler for the event's chord with context and event"
        @global => [:ctx "q"]
        "returns the global handler's truthy result"
        r => :quit)))

  (component "returns false when nothing handles the event"
    (let [tree (tui/vbox {:id "root"} (tui/button {:id "btn"} "B"))]
      (assertions
        "no node handler and no matching global chord yields false"
        (tui/route-key :ctx tree "btn" {:key :enter} {}) => false))))

;; --- process-key! driver against a synchronous raw app ---------------------

(m/defmutation set-driver-name [{:keys [v]}]
  (action [{:keys [state]}] (swap! state assoc :driver/name v)))

(tui/defsc DriverRoot [this {:keys [driver/name driver/hidden? driver/quit?]}]
  {:query         [:driver/name :driver/hidden? :driver/quit?]
   :ident         (fn [] [:component/id ::driver])
   :initial-state {:driver/name "AB" :driver/hidden? false :driver/quit? false}}
  (tui/vbox {:id "root"}
    (tui/input {:id        "name"
                :value     name
                :on-change (fn [v _caret] (tui/transact! this [(set-driver-name {:v v})]))})
    (when-not hidden?
      (tui/button {:id          "ok"
                   :on-focus    (fn [_] (tui/transact! this [(set-driver-name {:v "FOCUSED"})]))
                   :on-activate (fn [] (tui/transact! this [(set-driver-name {:v "ACTIVATED"})]))} "OK"))))

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
(tui/defsc MultilineRoot [this {:keys [driver/name driver/notes]}]
  {:query         [:driver/name :driver/notes]
   :ident         (fn [] [:component/id ::ml])
   :initial-state {:driver/name "AB" :driver/notes "NOTES"}}
  (tui/vbox {:id "root"}
    (tui/input {:id        "name"
                :value     name
                :on-change (fn [v _caret] (tui/transact! this [(set-driver-name {:v v})]))})
    (tui/input {:id         "notes"
                :multiline? true
                :value      notes
                :on-change  (fn [v _caret] (tui/transact! this [(set-driver-name {:v v})]))})
    (tui/button {:id "ok"} "OK")))

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

(specification {:covers {`tui/focused?      "17e934,8a7d8e"
                         `tui/current-focus "8b7584"
                         `tui/focus!        "f79a95"}} "focused? / current-focus / focus!"
  (let [app (build-driver-app)]
    (tui/focus! app "name")
    (assertions
      "focus! writes the focused id into the state-map at ::focus"
      (::tui/focus (deref (:com.fulcrologic.fulcro.application/state-atom app))) => "name"
      "current-focus reads the focused id back from the app"
      (tui/current-focus app) => "name"
      "current-focus also reads from a bare state-map"
      (tui/current-focus {::tui/focus "name"}) => "name"
      "focused? is true for the bound *current-focus*"
      (binding [tui/*current-focus* "name"] (tui/focused? "name")) => true
      "focused? is false for a different id"
      (binding [tui/*current-focus* "name"] (tui/focused? "other")) => false)))

(specification {:covers {`tui/process-key! "b42366,61c1a1"}} "process-key!"
  (component "Enter or Space activates a focused button"
    (let [app (build-driver-app)
          sa  (:com.fulcrologic.fulcro.application/state-atom app)]
      (assertions
        "Enter on a focused button fires its :on-activate (which transacts)"
        (do (tui/focus! app "ok") (tui/process-key! app {:key :enter}) (:driver/name @sa)) => "ACTIVATED"
        "Space on a focused button also fires :on-activate"
        (do (tui/focus! app "ok")
            (swap! sa assoc :driver/name "AB")
            (tui/process-key! app {:key " " :char " "})
            (:driver/name @sa)) => "ACTIVATED")))

  (component "Tab moves focus through the ring and wraps"
    (let [app (build-driver-app)]
      (tui/focus! app "name")
      (assertions
        "Tab advances focus to the next focusable"
        (do (tui/process-key! app {:key :tab}) (tui/current-focus app)) => "ok"
        "Tab from the last focusable wraps to the first"
        (do (tui/process-key! app {:key :tab}) (tui/current-focus app)) => "name"
        "Shift-Tab moves focus backward (wrapping)"
        (do (tui/process-key! app {:key :tab :shift? true}) (tui/current-focus app)) => "ok"
        ":backtab also moves focus backward"
        (do (tui/process-key! app {:key :backtab}) (tui/current-focus app)) => "name")))

  (component "typing into the focused input transacts the new value and advances the caret"
    (let [app (build-driver-app)
          sa  (:com.fulcrologic.fulcro.application/state-atom app)
          ra  (:com.fulcrologic.fulcro.application/runtime-atom app)]
      (tui/focus! app "name")
      (tui/process-key! app {:key "X" :char "X"})
      (assertions
        "the input's :on-change transaction updated the value in app state"
        (:driver/name @sa) => "ABX"
        "the caret store advanced to the new caret position"
        (get (::tui/carets @ra) "name") => 3)))

  (component ":on-focus fires when focus changes onto a node"
    (let [app (build-driver-app)
          sa  (:com.fulcrologic.fulcro.application/state-atom app)]
      (tui/focus! app "name")
      (tui/process-key! app {:key :tab})         ; name -> ok, fires ok's :on-focus
      (assertions
        "the newly focused node's :on-focus handler ran (its mutation changed state)"
        (:driver/name @sa) => "FOCUSED")))

  (component "focus is re-resolved when the focused node disappears"
    (let [app (build-driver-app)
          sa  (:com.fulcrologic.fulcro.application/state-atom app)]
      ;; focus "ok", then a global-keymap handler hides it; focus should move to "name"
      (tui/focus! app "ok")
      (tui/process-key! app {:key "h" :char "h"}
        {"h" (fn [a _e] (swap! (:com.fulcrologic.fulcro.application/state-atom a)
                          assoc :driver/hidden? true))})
      (assertions
        "after the focused node is removed, focus moves to a remaining focusable"
        (tui/current-focus app) => "name")))

  (component "a global quit chord handler fires for an unfocused-by-node event"
    (let [app    (build-driver-app)
          quit   (atom false)]
      (tui/focus! app "name")
      ;; ctrl-q is not consumed by the input editing path? input editing only runs for
      ;; the focused input's own keys; route-key is used when focus is not an input.
      (tui/focus! app "ok")                     ; focus the button (not an input)
      (tui/process-key! app {:key "q" :char "q" :ctrl? true}
        {[:ctrl "q"] (fn [_a _e] (reset! quit :quit))})
      (assertions
        "the global keymap handler ran for the ctrl-q chord"
        @quit => :quit)))

  (component "Down/Up arrows move focus through the ring (and wrap), like Tab/Shift-Tab"
    (let [app (build-driver-app)]
      (tui/focus! app "name")
      (assertions
        ":down advances focus to the next focusable"
        (do (tui/process-key! app {:key :down}) (tui/current-focus app)) => "ok"
        ":down from the last focusable wraps to the first"
        (do (tui/process-key! app {:key :down}) (tui/current-focus app)) => "name"
        ":up retreats focus to the previous focusable (wrapping)"
        (do (tui/process-key! app {:key :up}) (tui/current-focus app)) => "ok"
        ":up advances backward through the ring"
        (do (tui/process-key! app {:key :up}) (tui/current-focus app)) => "name")))

  (component "Down/Up on a single-line input navigate focus and leave the value unchanged"
    (let [app (build-multiline-app)
          sa  (:com.fulcrologic.fulcro.application/state-atom app)]
      (tui/focus! app "name")                   ; single-line input
      (tui/process-key! app {:key :down})
      (assertions
        "a single-line input releases :down to focus navigation (focus advances)"
        (tui/current-focus app) => "notes"
        "the single-line input's value was not edited by the navigation key"
        (:driver/name @sa) => "AB")))

  (component "a :multiline? input captures Up/Down instead of navigating focus"
    (let [app (build-multiline-app)]
      (tui/focus! app "notes")                  ; multiline input
      (tui/process-key! app {:key :down})
      (assertions
        ":down on a multiline input does not change focus (it is captured by the input)"
        (tui/current-focus app) => "notes")
      (tui/process-key! app {:key :up})
      (assertions
        ":up on a multiline input also does not change focus (it is captured)"
        (tui/current-focus app) => "notes"))))
