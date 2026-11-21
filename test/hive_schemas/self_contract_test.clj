(ns hive-schemas.self-contract-test
  "hive-schemas' contract-coverage gate, pointed at hive-schemas."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.coverage :as cov]
            [hive-schemas.instrument :as inst]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def ^:private gated-paths
  ["src" "synth"])

(def ^:private exempt
  {'hive-schemas.instrument/check-all
   "Generatively re-entrant: mg/check calls the original fn, which runs mi/check over the whole function-schema registry and reaches check-all again. The instrumentation wrapper cannot break the cycle because check reads the original var."

   'hive-schemas.instrument/check-violation
   "A one-branch delegate to check-all, so a contract on it recurses the generative checker through check-all for the same reason."})

(cov/deftest-contract-coverage every-public-fn-carries-a-contract
  {:paths  gated-paths
   :exempt exempt})

(deftest the-gate-is-not-vacuous
  (let [report (cov/coverage {:paths gated-paths :exempt exempt})]
    (is (<= 55 (count (:universe report)))
        (str "the universe collapsed to " (count (:universe report))
             " functions - a gate over nothing passes for the wrong reason"))
    (is (empty? (:unloadable report))
        (str "namespaces that failed to load, so their contracts were invisible: "
             (pr-str (mapv :ns (:unloadable report)))))))

(deftest every-contract-survives-instrumentation
  (let [covered (:covered (cov/coverage {:paths gated-paths :exempt exempt}))
        nss     (into [] (comp (map namespace) (map symbol) (distinct)) covered)
        wrapped (inst/instrument! {:ns nss})]
    (try
      (is (empty? (remove (into #{} (map symbol) wrapped) covered))
          "every covered public function should have been wrappable")
      (is (<= (count covered) (count wrapped))
          "private stages carry contracts too, so at least the public set is wrapped")
      (finally (inst/unstrument! {:ns nss})))))
