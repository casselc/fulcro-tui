(ns tui-demo.ui.root
  "Root and the routing outlet for the terminal demo. `Routes` is the statechart
   routing root (`:routing/root`); it renders whichever report/form is the active
   route. `Root` is the fulcro-tui app root and frames the routed content."
  (:require
    [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
    [com.fulcrologic.fulcro.tui.elements :as e]
    [com.fulcrologic.statecharts :as-alias sc]
    [com.fulcrologic.statecharts.integration.fulcro :as scf]
    [com.fulcrologic.statecharts.integration.fulcro.routing :as scr]))

(defsc Routes [this _props]
  {:query                   [:ui/current-route]
   :ident                   (fn [] [:component/id ::Routes])
   :preserve-dynamic-query? true
   :initial-state           {}}
  (scr/ui-current-subroute this comp/factory))

(def ui-routes (comp/factory Routes))

(defsc Root [this {:ui/keys [routes]}]
  {:query         [{:ui/routes (comp/get-query Routes)}
                   [::sc/session-id '_]]
   :initial-state {:ui/routes {}}}
  (e/vbox {:padding 1 :border? true :color :cyan}
    (e/text {:bold true} "Fulcro RAD + Statecharts — Terminal Demo  (Ctrl-Q quits)")
    (e/line {})
    (if (seq (scf/current-configuration this scr/session-id))
      (ui-routes routes)
      (e/text "Starting…"))))
