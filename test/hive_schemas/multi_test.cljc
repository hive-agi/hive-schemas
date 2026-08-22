(ns hive-schemas.multi-test
  "Proves the dispatch-seam bridge: the runtime levers (unit) plus the facets
   deftrifecta-from-multi synthesizes, running as real tests."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-schemas.test :as hst]
            [hive-spi.schema.registry :as reg]))

(reg/register! ::tone [:enum :warm :cool])

(def ^:private Tone
  "A CLOSED dispatch vocabulary."
  [:enum :warm :cool])

(def ^:private TintArgs
  "One call's argument list. `tint` is a defmulti, so this cannot ride on m/=>."
  [:cat [:map [:tone Tone]] :int])

(def ^:private Tinted
  [:map [:tone Tone] [:shift :int]])

(defmulti tint
  "Shift a tone. No :default: an unanticipated tone is a hard error."
  (fn [m _n] (:tone m)))

(defmethod tint :warm [m n] (assoc m :shift n))
(defmethod tint :cool [m n] (assoc m :shift (- n)))

(defn- plain-fn [x] x)

;; ------------------------------------------------------- synthesized facets

(hst/deftrifecta-from-multi tint-seam tint
  {:args     TintArgs
   :dispatch Tone
   :out      Tinted})

;; ---------------------------------------------------------- runtime levers

(deftest a-multimethod-is-not-a-fn
  (testing "the fact this whole path exists for: fn? REJECTS a multimethod, and
            malli's :=> validator demands fn?"
    (is (hst/multimethod? tint))
    (is (not (fn? tint)))
    (is (ifn? tint)))
  (testing "a plain fn is not a dispatch seam — it can take the m/=> path"
    (is (not (hst/multimethod? plain-fn)))
    (is (fn? plain-fn))))

(deftest dispatch-fn-refuses-a-non-multimethod
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo)
               (hst/dispatch-fn plain-fn))))

(deftest dispatch-vocabulary-reads-closed-schemas
  (testing ":enum, := and :or of those are closed"
    (is (= [:warm :cool] (hst/dispatch-vocabulary Tone)))
    (is (= [:warm]       (hst/dispatch-vocabulary [:= :warm])))
    (is (= [:warm :cool] (hst/dispatch-vocabulary [:or [:= :warm] [:= :cool]]))))
  (testing "a REGISTERED schema resolves through its indirection — properties
            live one m/deref down, so a lever that skips it is blind to exactly
            the schemas the house convention says to register"
    (is (= [:warm :cool]                          (hst/dispatch-vocabulary ::tone)))
    (is (= [:domestic :mercosur :international]
           (hst/dispatch-vocabulary (do (reg/register! ::zone [:enum :domestic :mercosur :international])
                                        ::zone))))))

(deftest dispatch-vocabulary-refuses-an-open-schema
  (testing "nil, not an empty vector: an OPEN dispatch set has no totality to
            check, and a caller that gates on one asserts nothing"
    (is (nil? (hst/dispatch-vocabulary :keyword)))
    (is (nil? (hst/dispatch-vocabulary :any)))
    (is (nil? (hst/dispatch-vocabulary [:or [:= :warm] :keyword])))))

(deftest undispatched-reads-the-DECLARED-vocabulary
  (testing "nothing missing when every declared value has a method"
    (is (empty? (hst/undispatched tint (hst/dispatch-vocabulary Tone)))))
  (testing "a value the vocabulary declares and the method table lacks is
            REPORTED — deriving the universe from the defmethods instead would
            report nothing, because it checks them against themselves"
    (is (= [:neon] (hst/undispatched tint [:warm :cool :neon])))))
