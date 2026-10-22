(ns hive-schemas.ansatz-contract-test
  "The ansatz lane's contract-coverage gate. The default `:test` lane cannot load
   ansatz/ (no kernel on its classpath), so the roots it skips are gated here."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.coverage :as cov]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def ^:private gated-paths
  ["ansatz"])

(cov/deftest-contract-coverage every-ansatz-lane-fn-carries-a-contract
  {:paths gated-paths})

(deftest the-ansatz-gate-is-not-vacuous
  (let [report (cov/coverage {:paths gated-paths})]
    (is (<= 3 (count (:universe report)))
        (str "the universe collapsed to " (count (:universe report))))
    (is (empty? (:unloadable report))
        (pr-str (mapv :ns (:unloadable report))))))
