(ns com.fulcrologic.fulcro.tui.hooks-integration-spec
  "Proves the TUI render pipeline establishes Fulcro's headless hook context, so React-style hooks
   (`use-state`) work in plain TUI components: their component-local state persists across renders
   (despite every render rebuilding the component instance) and is isolated per render-path (so the
   same component rendered twice — e.g. to-many subform rows — edits independently)."
  (:require
    [clojure.string :as str]
    [com.fulcrologic.fulcro.components :as comp]
    [com.fulcrologic.fulcro.react.hooks :as hooks]
    [com.fulcrologic.fulcro.tui.application :as app]
    [com.fulcrologic.fulcro.tui.elements :as elements]
    [com.fulcrologic.fulcro.tui.terminal :as term]
    [fulcro-spec.core :refer [=> assertions specification]]))

(comp/defsc Field
  "A leaf whose edited text lives ONLY in `use-state` (no app-db mutation), initialized to `init`."
  [this {:keys [id init]}]
  {:query [:id :init] :ident :id}
  (let [[s set-s!] (hooks/use-state init)]
    (elements/input {:id id :value s :on-change (fn [v _] (set-s! v))})))

(def ui-field (comp/factory Field {:keyfn :id}))

(comp/defsc OneRoot [this _]
  {:query [] :ident (fn [] [:component/id ::one]) :initial-state {}}
  (elements/vbox {:id "root"} (ui-field {:id :f :init ""})))

(comp/defsc TwoRoot [this _]
  {:query [] :ident (fn [] [:component/id ::two]) :initial-state {}}
  (elements/vbox {:id "root"}
    (ui-field {:id :f0 :init "A"})
    (ui-field {:id :f1 :init "B"})))

(specification "use-state in TUI components"
  (let [app (app/application {:root-class OneRoot :initial-state true})
        t   (term/string-terminal {:rows 4 :cols 20})]
    (app/attach! app t)                                     ; focuses :f
    (app/step! app {:key "H" :char "H"})
    (app/step! app {:key "i" :char "i"})
    (assertions
      "retains hook state across renders — typed text survives each rebuild (no snap-back to initial)"
      (str/trim (first (app/screen-of app))) => "Hi"))
  (let [app (app/application {:root-class TwoRoot :initial-state true})
        t   (term/string-terminal {:rows 4 :cols 20})]
    (app/attach! app t)                                     ; focuses :f0
    (app/step! app {:key "X" :char "X"})                    ; edit ONLY the first instance
    (assertions
      "isolates hook state per render-path — editing one instance does not change the other"
      (str/trim (first (app/screen-of app)))  => "AX"
      (str/trim (second (app/screen-of app))) => "B")))
