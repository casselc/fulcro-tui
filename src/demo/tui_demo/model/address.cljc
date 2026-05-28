(ns tui-demo.model.address
  "RAD attributes for the `address` entity. CLJC and babashka-safe. Addresses are
   stored as components of an account (see `tui-demo.model.account`)."
  (:require
    [com.fulcrologic.rad.attributes :refer [defattr]]
    [com.fulcrologic.rad.attributes-options :as ao]))

(defattr id :address/id :uuid
  {ao/identity? true
   ao/schema    :production})

(defattr street :address/street :string
  {ao/identities #{:address/id}
   ao/schema     :production})

(defattr city :address/city :string
  {ao/identities #{:address/id}
   ao/schema     :production})

(def states
  "US state enum: enum value -> two-letter label."
  #:address.state{:AL "AL" :AK "AK" :AZ "AZ" :AR "AR" :CA "CA" :CO "CO" :CT "CT"
                  :DE "DE" :DC "DC" :FL "FL" :GA "GA" :HI "HI" :ID "ID" :IL "IL"
                  :IN "IN" :IA "IA" :KS "KS" :KY "KY" :LA "LA" :ME "ME" :MD "MD"
                  :MA "MA" :MI "MI" :MN "MN" :MS "MS" :MO "MO" :MT "MT" :NE "NE"
                  :NV "NV" :NH "NH" :NJ "NJ" :NM "NM" :NY "NY" :NC "NC" :ND "ND"
                  :OH "OH" :OK "OK" :OR "OR" :PA "PA" :RI "RI" :SC "SC" :SD "SD"
                  :TN "TN" :TX "TX" :UT "UT" :VT "VT" :VA "VA" :WA "WA" :WV "WV"
                  :WI "WI" :WY "WY"})

(defattr state :address/state :enum
  {ao/identities        #{:address/id}
   ao/schema            :production
   ao/enumerated-values (set (keys states))
   ao/enumerated-labels states})

(defattr zip :address/zip :string
  {ao/identities #{:address/id}
   ao/schema     :production})

(def attributes [id street city state zip])
