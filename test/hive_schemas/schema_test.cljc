(ns hive-schemas.schema-test
  "Smoke test: the unified facade re-exports the hive-spi.schema levers and a
   registered schema round-trips through register! -> compile-op -> generate."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.schema :as s]))

(deftest facade-roundtrip-test
  (s/register! ::point [:map [:x :int] [:y :int]])
  (let [{:keys [validate coerce input-schema]} (s/compile-op ::point)]
    (is (validate {:x 1 :y 2}))
    (is (not (validate {:x 1})))
    (is (= {:x 30 :y 2} (coerce {:x "30" :y 2})))            ; json/string coercion
    (is (= "object" (:type input-schema)))                   ; JSON Schema shape
    (is (map? (s/generate ::point)))                         ; schema -> value
    (is (= 5 (count (s/sample ::point {:size 5}))))))        ; schema -> sample
