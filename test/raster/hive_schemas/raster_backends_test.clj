(ns hive-schemas.raster-backends-test
  "Rung-5 evidence that raster's WASM emitter agrees with a pure reference on an
   engine OUTSIDE the JVM.

   raster's own wasm suite executes emitted modules under Chicory and only
   Chicory. A module one engine accepts is not a module every engine accepts:
   validation strictness, legal-but-unusual encodings and opcode coverage all
   differ. This runs the same emitted module under zwasm, through the cljw
   binary, on inputs generated from the kernel's own malli schema.

   The binary arrives through CLJW_BIN, defaulting to `cljw` on PATH — a machine
   path is a collaborator, never a literal."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.test.check.generators :as gen]
            [malli.generator :as mg]
            [hive-schemas.raster :as hr]
            [hive-schemas.raster.backends :as bk]
            [raster.dl.nn :as nn]
            [hive-schemas.raster.backends.chicory :as ch]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def cljw-binary
  "The cljw binary, through a seam: CLJW_BIN, then the `cljw.bin` system
   property, then `cljw` on PATH. A machine path is a collaborator, never a
   literal."
  (or (System/getenv "CLJW_BIN") (System/getProperty "cljw.bin") "cljw"))

(bk/register-backend! (ch/chicory-backend))

(bk/register-backend! (bk/cljw-backend cljw-binary :interp))
(bk/register-backend! (bk/cljw-backend cljw-binary :jit))

(def missing-backend-remedy
  (str "no wasm backend ran. Point CLJW_BIN (or -Dcljw.bin) at a cljw binary "
       "carrying the LINEAR-MEMORY surface — `--version` reporting `wasm` is not "
       "enough, an older build resolves wasm/load and then dies on "
       "wasm/mem-write!. A differential test with no second engine has not "
       "passed, it has not been run."))

(def residual-overload (first (hr/overloads #'nn/residual-add!)))

(defn- f32-round
  "Round through f32 so the JVM reference and the f32 kernel are comparing the
   same values, not the emitter against a rounding step."
  [xs]
  (mapv #(double (float %)) xs))

(defn- residual-plan [n a b]
  {:export "residual_add"
   :args   [0 (* 4 n) (* 8 n) n]
   :writes [{:at 0 :dtype :f32 :values a}
            {:at (* 4 n) :dtype :f32 :values b}]
   :reads  [{:at (* 8 n) :dtype :f32 :count n}]})

(deftest backend-availability-is-measured-not-assumed
  (testing "Chicory and both zwasm engines are registered"
    (is (= #{:chicory :cljw-interp :cljw-jit} (set (keys (bk/backends))))))
  (testing "and availability comes from RUNNING each one, not from a classpath
           entry or an env var being set"
    (is (seq (bk/available-backends)) missing-backend-remedy)))

(deftest residual-add-agrees-with-zwasm-on-schema-generated-inputs
  (let [backend (get (bk/available-backends) :cljw-interp)]
    (is (some? backend) missing-backend-remedy)
    (when backend
      (let [sch   (hr/arglist-schema residual-overload
                                     {:lengths (hr/infer-lengths residual-overload)
                                      :min-len 1 :max-len 12 :magnitude 64.0})
            bytes (bk/module-bytes
                   (bk/compile-plan #'nn/residual-add! "residual_add" {:dtype :float}))
            cases (take 6 (gen/sample (mg/generator sch) 12))]
        (is (seq cases))
        (doseq [[a b _out n] cases]
          (let [af       (f32-round (take n a))
                bf       (f32-round (take n b))
                expected (mapv + af bf)
                result   (bk/run-plan backend bytes (residual-plan n af bf))
                got      (first (:reads result))]
            (is (nil? (bk/call-error result))
                (str "zwasm refused the call: " (bk/call-error result)))
            (is (pos? n) ":min-len 1 keeps the comparison non-empty")
            (is (some (complement zero?) expected)
                "every expected element cancelled to zero — this case cannot tell
                 a correct kernel from one that never wrote the buffer")
            (is (hr/approx-seq= expected got 1.0e-6)
                (str "n=" n " expected " expected " got " got))))))))

(deftest two-independent-wasm-engines-agree-on-schema-generated-inputs
  (testing "Chicory and zwasm are separate implementations of the same spec, so
           a module one accepts is not a module every engine accepts — validation
           strictness, legal-but-unusual encodings and opcode coverage all
           differ. Agreement between two is the evidence; disagreement on an
           identical module and identical inputs is an emitter bug"
    (let [engines (select-keys (bk/available-backends) [:chicory :cljw-interp])]
      (is (= #{:chicory :cljw-interp} (set (keys engines)))
          (str "a differential test needs BOTH engines. " missing-backend-remedy))
      (when (= 2 (count engines))
        (let [sch   (hr/arglist-schema residual-overload
                                       {:lengths (hr/infer-lengths residual-overload)
                                        :min-len 1 :max-len 12 :magnitude 64.0})
              bytes (bk/module-bytes
                     (bk/compile-plan #'nn/residual-add! "residual_add" {:dtype :float}))
              cases (take 5 (gen/sample (mg/generator sch) 12))]
          (is (seq cases))
          (doseq [[a b _out n] cases]
            (let [af   (f32-round (take n a))
                  bf   (f32-round (take n b))
                  outs (into {} (for [[id e] engines]
                                  [id (first (:reads (bk/run-plan e bytes
                                                                  (residual-plan n af bf))))]))]
              (is (some (complement zero?) (mapv + af bf))
                  "every element cancelled — this case cannot separate a correct
                   kernel from one that never wrote the buffer")
              (is (apply = (vals outs))
                  (str "the engines disagree at n=" n ": " outs))
              (is (hr/approx-seq= (mapv + af bf) (get outs :chicory) 1.0e-6)
                  (str "and both disagree with the reference at n=" n)))))))))

(deftest zwasm-jit-still-refuses-a-void-arity-4-export
  (testing "ClojureWasm D-585: zwasm's JIT traps at INVOKE on a zero-result
           export outside a narrow signature window. The module BUILDS, so
           `.auto` never downgrades, and `(i32,i32,i32,i32) -> ()` is exactly
           what an in-place numeric kernel is. This is why the oracle above runs
           on :interp; pinned here so the workaround is removed when zwasm
           stops needing it, rather than outliving the defect"
    (let [backend (get (bk/available-backends) :cljw-jit)]
      (is (some? backend) missing-backend-remedy)
      (when backend
        (let [bytes  (bk/module-bytes
                      (bk/compile-plan #'nn/residual-add! "residual_add" {:dtype :float}))
              result (bk/run-plan backend bytes
                                  (residual-plan 4 [0.5 -1.5 2.25 3.0] [0.25 -0.5 1.5 4.0]))]
          (is (re-find #"trapped" (or (bk/call-error result) ""))
              (str "the JIT no longer refuses this shape — re-measure D-585 and "
                   "drop the :interp routing if it is fixed. call=" (:call result))))))))

(deftest the-two-engines-disagree-here-and-that-is-the-point
  (testing "one engine is not a differential test: :interp computes the kernel
           and :jit refuses it, on the identical module and identical inputs"
    (let [avail (bk/available-backends)
          i (get avail :cljw-interp)
          j (get avail :cljw-jit)]
      (is (and i j) missing-backend-remedy)
      (when (and i j)
        (let [bytes (bk/module-bytes
                     (bk/compile-plan #'nn/residual-add! "residual_add" {:dtype :float}))
              plan  (residual-plan 4 [0.5 -1.5 2.25 3.0] [0.25 -0.5 1.5 4.0])
              ri    (bk/run-plan i bytes plan)
              rj    (bk/run-plan j bytes plan)]
          (is (= [[0.75 -2.0 3.75 7.0]] (:reads ri)))
          (is (not= (:reads ri) (:reads rj))
              "if these agree, either the JIT was fixed or the reads are not
               observing the kernel at all"))))))
