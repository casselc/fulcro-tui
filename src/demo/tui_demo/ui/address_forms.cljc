(ns tui-demo.ui.address-forms
  "RAD (statechart) form for an Address. Used standalone and as a component subform of
   an Account (primary address + additional addresses)."
  (:require
    [com.fulcrologic.rad.form-options :as fo]
    [com.fulcrologic.rad.statechart.form :refer [defsc-form]]
    [tui-demo.model.address :as address]))

(defsc-form AddressForm [this props]
  {fo/id         address/id
   fo/attributes [address/street address/city address/state address/zip]
   fo/title      "Address"
   fo/layout     [[:address/street]
                  [:address/city :address/state :address/zip]]})
