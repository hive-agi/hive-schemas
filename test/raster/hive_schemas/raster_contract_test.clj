(ns hive-schemas.raster-contract-test
  "The raster lane's contract-coverage gate and hive-test facets.

   The default `:test` lane gates src/ + synth/ only, because raster/ and typed/
   do not load without raster on the classpath. This lane has it, so it gates
   those roots itself — otherwise their contracts are checked by nobody."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.coverage :as cov]
            [hive-schemas.raster :as hr]
            [hive-schemas.raster.hostgen :as hg]
            [hive-schemas.test :as hst]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def ^:private gated-paths
  ["raster" "typed"])

(cov/deftest-contract-coverage every-raster-lane-fn-carries-a-contract
  {:paths gated-paths})

(deftest the-raster-gate-is-not-vacuous
  (let [report (cov/coverage {:paths gated-paths})]
    (is (<= 40 (count (:universe report)))
        (str "the universe collapsed to " (count (:universe report))))
    (is (empty? (:unloadable report))
        (pr-str (mapv :ns (:unloadable report))))))

;; =============================================================================
;; hostgen
;; =============================================================================

(def ^:private Sym
  "A small symbol domain, so generated call entries collide with the layout."
  [:enum 'A 'B 'C 'n 'xs])

(def ^:private CallEntry
  "One `:call` entry over `Sym`."
  [:or Sym
   [:tuple [:= :lit] :int]
   [:tuple [:enum :const :resident] Sym]
   [:tuple [:enum :in :inout] Sym Sym]])

(defn marshalled-rel
  "Exactly the :in / :inout entries, in call order."
  [call out]
  (= out (filterv #(and (vector? %) (contains? #{:in :inout} (first %))) call)))

(hst/deftrifecta-from-schema marshalled-facets hg/marshalled
  {:in        [:vector CallEntry]
   :out       [:vector [:tuple [:enum :in :inout] :symbol :symbol]]
   :rel       hive-schemas.raster-contract-test/marshalled-rel
   :contract  true
   :num-tests 60})

(def fixed-layout
  "A three-region layout: a 12-byte const, a 24-byte scratch, a 4-byte resident."
  (hg/lay-out [{:sym 'A :data [1 2 3] :view :i32}]
              [{:sym 'B :view :f64 :bytes 24}]
              [{:sym 'C :view :f32 :bytes 4}]))

(defn byte-off-rel
  "The offset is the layout entry's, and every offset is 8-aligned."
  [[layout sym] out]
  (and (= out (get-in layout [sym :off]))
       (or (nil? out) (zero? (mod out 8)))))

(hst/deftrifecta-from-schema byte-off-facets hg/byte-off
  {:in        [:cat [:= {} fixed-layout] Sym]
   :out       [:maybe :int]
   :rel       hive-schemas.raster-contract-test/byte-off-rel
   :contract  true
   :num-tests 40})

;; =============================================================================
;; raster
;; =============================================================================

(defn schema->tag-rel
  "Only the canonical scalar schemas map back to a tag."
  [s out]
  (= out (get {:double 'double :int 'long :boolean 'boolean :nil 'void} s)))

(hst/deftrifecta-from-schema schema->tag-facets hr/schema->tag
  {:in        [:enum :double :int :boolean :nil :string :keyword]
   :out       [:maybe :symbol]
   :rel       hive-schemas.raster-contract-test/schema->tag-rel
   :contract  true
   :num-tests 40})

(defn approx-seq=-rel
  "Equal length and every pair within the default tolerance."
  [[a b] out]
  (= out (and (= (count a) (count b))
              (every? true? (map #(hr/approx= %1 %2) a b)))))

(hst/deftrifecta-from-schema approx-seq=-facets hr/approx-seq=
  {:in        [:cat [:vector {:max 4} [:double {:min -4.0 :max 4.0}]]
               [:vector {:max 4} [:double {:min -4.0 :max 4.0}]]]
   :out       :boolean
   :rel       hive-schemas.raster-contract-test/approx-seq=-rel
   :contract  true
   :num-tests 60})
