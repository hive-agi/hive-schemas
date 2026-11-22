(ns hive-schemas.schema-test
  "Smoke test: the unified facade re-exports the hive-spi.schema levers and a
   registered schema round-trips through register! -> compile-op -> generate."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.schema :as s]
            [hive-schemas.test :as hst]))

(defn validate-rel
  "The facade answers exactly what malli answers for an :int schema."
  [[_ x] out]
  (= out (int? x)))

(hst/deftrifecta-from-schema validate-facets s/validate
  {:in       [:cat [:= :int] [:or :int :string :nil]]
   :out      :boolean
   :rel      hive-schemas.schema-test/validate-rel
   :contract true
   :num-tests 40})

(deftest facade-roundtrip-test
  (s/register! ::point [:map [:x :int] [:y :int]])
  (let [{:keys [validate coerce input-schema]} (s/compile-op ::point)]
    (is (validate {:x 1 :y 2}))
    (is (not (validate {:x 1})))
    (is (= {:x 30 :y 2} (coerce {:x "30" :y 2})))            ; json/string coercion
    (is (= "object" (:type input-schema)))                   ; JSON Schema shape
    (is (map? (s/generate ::point)))                         ; schema -> value
    (is (= 5 (count (s/sample ::point {:size 5}))))))        ; schema -> sample
