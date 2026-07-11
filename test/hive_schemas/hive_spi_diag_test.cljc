(ns hive-schemas.hive-spi-diag-test
  "Bridge v2 fanned out over a SECOND real hive-spi subsystem: hive-spi.diag.
   Three distinct shapes exercise the bridge beyond the WorkflowAST ops:

     - human-bytes   a SCALAR-output fn (byte count -> human string). No map keys
                     to mutate, so teeth come from :rel + :contract (mg/check),
                     with :mutation false — the bridge handles non-map outputs.
     - ->heap-snapshot / ->reclamation   smart constructors that VALIDATE and
                     return their input (identity on valid), so :rel is equality
                     and the mutation facet corrupts the pinned record schema.
     - heap-valid?   the diag validator in predicate mode (accepts valid heaps,
                     rejects schema-corrupted ones)."
  (:require [clojure.test :refer [deftest is]]
            [hive-spi.diag.schema :as diag]
            [hive-spi.schema.registry :as reg]
            [hive-schemas.test :as hst]))

;; --- the schemas the diag ops speak ---
(reg/register! ::nn-long diag/NonNegLong)      ; [:maybe [:int {:min 0}]]
(reg/register! ::heap    diag/HeapSnapshot)
(reg/register! ::recl    diag/Reclamation)

;; --- scalar output: byte count -> "N.NN <unit>" (or "n/a" for nil) ---
(defn human-rel [n out]
  (if (nil? n)
    (= "n/a" out)
    (boolean (re-matches #"\d+(\.\d+)? (B|KiB|MiB|GiB|TiB)" out))))

(hst/deftrifecta-from-schema human-bytes-tests hive-spi.diag.schema/human-bytes
  {:in ::nn-long :out :string :rel human-rel :contract true :mutation false
   :num-tests 60})

;; --- smart constructors: identity on valid input, mutation over the record ---
(hst/deftrifecta-from-schema ->heap-tests hive-spi.diag.schema/->heap-snapshot
  {:in ::heap :out ::heap :rel (fn [in out] (= in out)) :contract true :num-tests 30})

(hst/deftrifecta-from-schema ->recl-tests hive-spi.diag.schema/->reclamation
  {:in ::recl :out ::recl :rel (fn [in out] (= in out)) :contract true :num-tests 30})

;; --- validator in predicate mode ---
(defn heap-valid? [x] (diag/valid? diag/HeapSnapshot x))

(hst/deftrifecta-predicate heap-valid?-tests hive-schemas.hive-spi-diag-test/heap-valid?
  {:schema ::heap :num-tests 40})

;; --- non-vacuity guard: the record oracle still has teeth on required keys ---
(deftest diag-teeth-guard-test
  (let [ok?   (hst/output-oracle ::heap)
        good  (diag/->heap-snapshot {:diag/used-bytes 1 :diag/committed-bytes 2 :diag/max-bytes 3})]
    (is (ok? good))                                             ; a real snapshot conforms
    (is (not (ok? (dissoc good :diag/used-bytes))))            ; dropped required key rejected
    (is (pos? (count (hst/schema-mutants diag/->heap-snapshot ::heap))))))
