(ns hive-schemas.raster-test
  "Rung-4/5 evidence for the raster bridge, against REAL raster kernels.

   Kernels chosen to cover the shapes the bridge distinguishes:
     raster.dl.loss/l1-loss-backward  concrete, coupled, array return
     raster.dl.loss/mse-loss          parametric (All [T]), coupled, scalar return
     raster.par/cumsum                concrete, UNcoupled, array in and out
     raster.knn/l2-normalize!         coupled, two extents, so inference declines
     raster.numeric/pow               four overloads, so selection is ambiguous"
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.test.check.generators :as gen]
            [malli.core :as m]
            [malli.generator :as mg]
            [hive-schemas.raster :as hr]
            [hive-schemas.raster.synth :as hrs]
            [hive-schemas.typed-check :as tc]
            [raster.dl.loss :as loss]
            [raster.knn :as knn]
            [raster.numeric :as rnum]
            [raster.par :as par]
            [raster.core :refer [deftm]]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

;; ---------------------------------------------------------------------------
;; Pure references. Each is the ORACLE its kernel is compared against, written
;; from the kernel's documented meaning rather than from its implementation.
;; ---------------------------------------------------------------------------

(defn l1-backward-ref
  "d-pred[i] = dy * signum(pred[i] - target[i]) / n"
  [dy pred target n]
  (mapv (fn [i] (* dy (/ (Math/signum (- (aget ^doubles pred i) (aget ^doubles target i)))
                         (double n))))
        (range n)))

(defn mse-ref
  "mean((pred - target)^2) over n elements."
  [pred target n]
  (/ (reduce + (map (fn [p t] (let [d (- p t)] (* d d))) (seq pred) (seq target)))
     (double n)))

(defn cumsum-ref
  "Inclusive prefix sum. Empty in, empty out."
  [a]
  (vec (rest (reductions + 0 (seq a)))))

;; ---------------------------------------------------------------------------
;; Fixtures
;; ---------------------------------------------------------------------------

(def l1-overload     (first (hr/overloads #'loss/l1-loss-backward)))
(def cumsum-overload (first (hr/overloads #'par/cumsum)))
(def l2norm-overload (first (hr/overloads #'knn/l2-normalize!)))

;; `All` and `Array` are deftm's macro vocabulary, not vars — they are consumed
;; by the macro and must NOT be :refer'd.
(deftm probe-scale (All [T] [alpha :- T, x :- T] :- T (rnum/* alpha x)))

(def l1-arglist
  (hr/arglist-schema l1-overload {:lengths (hr/infer-lengths l1-overload)
                                  :max-len 8 :magnitude 100.0}))

(def independent-arglist
  "The baseline `arglist-schema` REFUSES to emit: the arrays and the length drawn
   independently. Only ever generated from — it exists to show the consistency
   property below is not true of any generator."
  (m/schema [:catn
             [:dy     (hr/scalar-schema 'double {:magnitude 100.0})]
             [:pred   (hr/array-schema 'doubles {:max-len 8 :magnitude 100.0})]
             [:target (hr/array-schema 'doubles {:max-len 8 :magnitude 100.0})]
             [:n      (hr/scalar-schema 'long {:magnitude 8})]]))

(defn- samples [schema n] (gen/sample (mg/generator schema) n))
(defn- nonempty [ss] (remove (fn [args] (zero? (last args))) ss))

;; ---------------------------------------------------------------------------
;; Recognizing a deftm, and enumerating its overloads
;; ---------------------------------------------------------------------------

(deftest deftm-recognition
  (testing "the dispatch var and its backing impl var are both recognized"
    (is (hr/deftm? #'loss/l1-loss-backward))
    (is (hr/deftm? (:impl-var l1-overload))))
  (testing "a plain defn var is not"
    (is (not (hr/deftm? #'hr/coupled?)))
    (is (not (hr/deftm? #'clojure.core/inc)))))

(deftest overloads-of-a-concrete-deftm
  (is (= 1 (count (hr/overloads #'loss/l1-loss-backward))))
  (is (= '[dy pred target n] (:params l1-overload)))
  (is (= '[double doubles doubles long] (:tags l1-overload)))
  (is (= 'doubles (:ret l1-overload))))

(deftest overloads-of-a-parametric-deftm
  (testing "an (All [T]) deftm registers its double monomorphization EAGERLY, so
           enumerating without :dtype is never empty"
    (is (contains? (set (mapv :tags (hr/overloads #'loss/mse-loss)))
                   '[doubles doubles long])))
  (testing ":dtype pins exactly one, and reaches the float instantiation"
    (is (= '[[floats floats long]]
           (mapv :tags (hr/overloads #'loss/mse-loss {:dtype :float}))))))

(deftest dtype-monomorphization-mutates-the-shared-dispatch-table
  (testing "a fresh (All [T]) deftm arrives with its double instantiation already
           registered — nothing had to ask for it"
    (is (= '[[double double]] (mapv :tags (hr/overloads #'probe-scale)))))
  (testing "asking for :float REGISTERS it, so the next :dtype-less enumeration
           is wider: `overloads` reports the table as it stands in this image,
           not a property of the definition"
    (is (= '[[float float]] (mapv :tags (hr/overloads #'probe-scale {:dtype :float}))))
    (is (= #{'[double double] '[float float]}
           (set (mapv :tags (hr/overloads #'probe-scale)))))))

(deftest overloads-refuses-a-non-deftm-var
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not a raster deftm var"
                        (hr/overloads #'hr/coupled?))))

;; ---------------------------------------------------------------------------
;; Coupling: an array parameter and its extent are not independent
;; ---------------------------------------------------------------------------

(deftest coupling-detection
  (is (true? (hr/coupled? l1-overload))
      "coupled? must answer a boolean, not `and`'s last truthy value")
  (is (false? (hr/coupled? cumsum-overload)))
  (is (= '{n #{pred target}} (hr/infer-lengths l1-overload)))
  (is (nil? (hr/infer-lengths cumsum-overload))
      "no integer parameter, so nothing governs an extent"))

(deftest infer-lengths-declines-a-multi-extent-signature
  (testing "l2-normalize! is coupled but has TWO extents (n and dim), so no
           single parameter governs every array"
    (is (true? (hr/coupled? l2norm-overload)))
    (is (nil? (hr/infer-lengths l2norm-overload))))
  (testing "and a matmul-shaped signature declines for the same reason"
    (is (nil? (hr/infer-lengths {:name 'matmul :params '[A B m k n]
                                 :tags '[doubles doubles long long long]
                                 :ret 'doubles})))))

(deftest arglist-schema-refuses-a-coupled-overload-without-lengths
  (let [e (try (hr/arglist-schema l1-overload {}) nil
               (catch clojure.lang.ExceptionInfo e e))]
    (is (some? e) "a coupled overload with no :lengths must refuse, not guess")
    (is (re-find #"couples an array parameter" (ex-message e)))
    (is (= '{n #{pred target}} (:inferred (ex-data e)))
        "the refusal reports the inference it declined to apply silently")))

(deftest coupled-generator-keeps-every-extent-consistent
  (let [ss (samples l1-arglist 100)]
    (is (every? (fn [[_ pred target n]]
                  (= n (alength ^doubles pred) (alength ^doubles target)))
                ss))
    (is (every? #(m/validate l1-arglist %) ss))))

(deftest coupled-generator-is-not-vacuous
  (testing "some sample has n > 0 — consistency over only empty arrays asserts nothing"
    (is (pos? (count (nonempty (samples l1-arglist 100))))))
  (testing "and the consistency property DISCRIMINATES: drawing the arrays and
           the extent independently breaks it at a positive rate"
    (is (pos? (count (remove (fn [[_ pred target n]]
                               (= n (alength ^doubles pred) (alength ^doubles target)))
                             (samples independent-arglist 100)))))))

;; ---------------------------------------------------------------------------
;; Rung 5: generated arguments through the real kernel, against the reference
;; ---------------------------------------------------------------------------

(deftest differential-oracle-l1-backward
  (let [ss (nonempty (samples l1-arglist 100))]
    (is (seq ss))
    (is (every? (fn [args] (hr/approx-seq= (vec (apply loss/l1-loss-backward args))
                                           (apply l1-backward-ref args)
                                           1.0e-12))
                ss))))

(deftest differential-oracle-discriminates
  (testing "the same comparison REJECTS a reference with the sign flipped —
           without this the agreement above could be an artifact of the oracle"
    (let [flipped (hr/approx-rel (fn [& args] (mapv - (apply l1-backward-ref args)))
                                 {:eps 1.0e-12})
          ss      (nonempty (samples l1-arglist 100))]
      (is (pos? (count (remove (fn [args] (flipped args (apply loss/l1-loss-backward args)))
                               ss)))
          "a sign-flipped reference passed — the oracle asserts nothing"))))

(deftest differential-oracle-mse-at-double
  (let [ov  (first (hr/overloads #'loss/mse-loss {:dtype :double}))
        sch (hr/arglist-schema ov {:lengths (hr/infer-lengths ov)
                                   :min-len 1 :max-len 8 :magnitude 100.0})
        ss  (samples sch 60)]
    (is (every? (fn [[_ _ n]] (pos? n)) ss)
        ":min-len 1 keeps mean-of-n defined — at n=0 both sides are NaN and
         approx= is correctly false, which would look like a failing kernel")
    (is (every? (fn [args] (hr/approx= (apply loss/mse-loss args)
                                       (apply mse-ref args)
                                       1.0e-9))
                ss))))

(deftest an-uncoupled-overload-needs-no-lengths
  (let [sch (hr/arglist-schema cumsum-overload {:max-len 12 :magnitude 100.0})
        ss  (samples sch 40)]
    (is (every? (fn [args] (hr/approx-seq= (vec (apply par/cumsum args))
                                           (apply cumsum-ref args)
                                           1.0e-9))
                ss))))

;; ---------------------------------------------------------------------------
;; The float comparators
;; ---------------------------------------------------------------------------

(deftest approx=-is-absolute-below-the-crossover-and-relative-above
  (testing "absolute: two tiny numbers agree though their RATIO is 2"
    (is (hr/approx= 1.0e-12 2.0e-12 1.0e-9)))
  (testing "relative: two large numbers agree when their ratio is within eps"
    (is (hr/approx= 1.0e9 (+ 1.0e9 0.5) 1.0e-9))
    (is (not (hr/approx= 1.0e9 (+ 1.0e9 100.0) 1.0e-9))))
  (testing "NaN is not equal to itself, as IEEE says"
    (is (not (hr/approx= ##NaN ##NaN)))))

(deftest approx-seq=-on-a-length-mismatch-is-false-not-a-throw
  (is (false? (hr/approx-seq= [1.0 2.0] [1.0])))
  (is (hr/approx-seq= (double-array [1.0 2.0]) [1.0 2.0]) "a Java array is seqable")
  (is (hr/approx-seq= [] [])))

;; ---------------------------------------------------------------------------
;; The reverse direction: malli schema -> deftm signature
;; ---------------------------------------------------------------------------

(deftest overload-annotations-reconstruct-the-signature
  (is (= '[dy :- Double pred :- (Array double) target :- (Array double) n :- Long]
         (hr/overload-annotations l1-overload)))
  (is (= '(Array double) (hr/return-annotation l1-overload))))

(deftest a-generated-schema-round-trips-back-to-its-own-signature
  (testing "exact at :double"
    (let [ov  (first (hr/overloads #'loss/mse-loss {:dtype :double}))
          sch (hr/arglist-schema ov {:lengths (hr/infer-lengths ov)})]
      (is (= (hr/overload-annotations ov) (hr/param-annotations sch)))))
  (testing "exact at :float TOO — the schema carries :raster/tag, so the reverse
           direction reads the tag back instead of canonicalizing to double"
    (let [ov  (first (hr/overloads #'loss/mse-loss {:dtype :float}))
          sch (hr/arglist-schema ov {:lengths (hr/infer-lengths ov)})]
      (is (= '[pred :- (Array float) target :- (Array float) n :- Long]
             (hr/param-annotations sch)))
      (is (= (hr/overload-annotations ov) (hr/param-annotations sch))))))

(deftest a-hand-written-schema-inverts-canonically-and-lossily
  (testing "no :raster/tag provenance, so :double canonicalizes to Double and
           :int to Long — Float and the int arrays are unreachable this way"
    (is (= '[x :- Double xs :- (Array double) k :- Long]
           (hr/param-annotations
            (m/schema [:catn [:x :double] [:xs [:vector :double]] [:k :int]])))))
  (testing "a schema no raster type corresponds to is refused, not guessed"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no raster annotation"
                          (hr/param-annotations (m/schema [:catn [:s :string]])))))
  (testing "and a non-arglist schema is refused before anything is inferred"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"needs a :catn arglist"
                          (hr/param-annotations (m/schema [:vector :double]))))))

;; ---------------------------------------------------------------------------
;; kernel-schema: the single-overload selection defkernel-trifecta rests on
;; ---------------------------------------------------------------------------

(deftest kernel-schema-resolves-lengths-on-request
  (is (= #{:in :out}
         (set (keys (hr/kernel-schema #'loss/l1-loss-backward {:lengths :infer}))))))

(deftest kernel-schema-refuses-what-it-cannot-decide
  (testing "four overloads and no :dtype — nothing says which one"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"needs exactly one overload"
                          (hr/kernel-schema #'rnum/pow {}))))
  (testing ":lengths :infer over a two-extent signature — inference declines and
           the refusal is propagated rather than swallowed"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"infer-lengths declined"
                          (hr/kernel-schema #'knn/l2-normalize! {:lengths :infer})))))

;; ---------------------------------------------------------------------------
;; Rung D: the Typed Clojure checker, and the pass that means nothing
;; ---------------------------------------------------------------------------

(deftest typed-check-refuses-a-namespace-the-checker-skipped
  (testing "check-ns-info answers an EMPTY :type-errors for a namespace with no
           ^:typed.clojure opt-in — byte-identical to a clean check"
    (is (empty? (tc/type-errors 'hive-schemas.vocab))))
  (testing "so an unchecked namespace has to be a violation, not a green row"
    (is (re-find #"was NOT CHECKED" (tc/check-violation 'hive-schemas.vocab)))))

(deftest the-tc-fixture-checks-clean-once-raster-extensions-are-prepared
  (testing "(* 32 64) propagates to (t/Val 2048) with raster's checker
           extensions loaded. Measured cold WITHOUT them, the same namespace
           reports `Expected (t/Val 2048), Actual Long` — this suite has raster
           loaded, which pulls tc-extensions transitively, so :prepare makes
           explicit what would otherwise be accidental"
    (is (nil? (tc/check-violation 'hive-schemas.raster-tc-fixture
                                  {:prepare [hr/tc-extensions]}))))
  (testing "and the fixture IS opted in, so that nil means checked-and-clean"
    (is (true? (tc/opted-in? 'hive-schemas.raster-tc-fixture)))))

;; ---------------------------------------------------------------------------
;; The whole ladder, synthesized from the signature alone
;; ---------------------------------------------------------------------------

(hrs/defkernel-trifecta l1-backward loss/l1-loss-backward
  {:lengths   :infer
   :max-len   8
   :magnitude 100.0
   :eps       1.0e-12
   :reference l1-backward-ref
   :num-tests 50})
