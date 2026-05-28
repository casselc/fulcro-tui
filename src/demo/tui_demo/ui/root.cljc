(ns tui-demo.ui.root
  "Root and the routing outlet for the terminal demo. `Routes` is the statechart
   routing root (`:routing/root`); it renders whichever report/form is the active
   route. `Root` is the fulcro-tui app root and frames the routed content."
  (:require
    [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
    [com.fulcrologic.fulcro.tui.application :as tui-app]
    [com.fulcrologic.fulcro.tui.elements :as e]
    [com.fulcrologic.fulcro.tui.engine :as engine]
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
  (let [err       (some-> comp/*app* tui-app/last-error)
        enhanced? engine/*enhanced-keys?*]
    (e/vbox {:padding 1 :border? true :color :cyan}
      (e/text {:bold true} "Fulcro RAD + Statecharts — Terminal Demo  (Ctrl-Q quit · Ctrl-L redraw)")
      ;; Shortcut layer status: Alt-* control shortcuts and mnemonic underlines only work when the
      ;; terminal negotiated the enhanced (Kitty/CSI-u) keyboard protocol at startup.
      (e/text {:color (if enhanced? :green :bright-red)}
        (if enhanced?
          "shortcuts: ON (enhanced keyboard) — Alt-s/u/c, Alt-p/n, Alt-c, Alt-j/k"
          "shortcuts: OFF — terminal lacks the enhanced (Kitty/CSI-u) keyboard protocol"))
      (e/line {})
      (if (seq (scf/current-configuration this scr/session-id))
        (ui-routes routes)
        (e/text "Starting…"))
      ;; Sane error reporting: a tolerated input/render error is shown here (in-UI, so it never
      ;; corrupts the alt-screen) rather than crashing the session. Ctrl-L forces a clean redraw.
      (when err
        (e/text {:color :bright-red}
          (str "⚠ " (ex-message err) " — press Ctrl-L to redraw"))))))
