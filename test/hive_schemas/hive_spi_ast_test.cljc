(ns hive-schemas.hive-spi-ast-test
  "Bridge v2 proven on REAL hive-spi ops — the full 7 WorkflowAST smart
   constructors + ->edn + edn->ast + valid?. Every facet is synthesized from
   hive-spi.workflow.ast's own malli schemas. Teeth come from three sources the
   adversarial review demanded: PINNED :out schemas (op identity), RELATIONAL
   :rel predicates (output relates to input — child count, id paths, arg
   preservation), and PREDICATE mode (valid? accepts valid, rejects corrupted).
   Exercises recursive WorkflowAST on both input and output."
  (:require [clojure.test :refer [deftest is]]
            [hive-spi.workflow.ast :as ast]
            [hive-spi.schema.registry :as reg]
            [hive-schemas.test :as hst]))

;; --- schemas the ops speak ---
(reg/register! ::node      ast/WorkflowAST)
(reg/register! ::args      [:map-of :any :any])
(reg/register! ::call-args [:map [:verb :any]])
(reg/register! ::gate-args [:map [:when :keyword]])
(reg/register! ::let-args  [:map [:bindings [:vector :any]]])
(reg/register! ::children  [:vector {:min 1 :max 3} [:ref ::node]])
;; pinned output nodes — the op tag is CONSTRAINED, not just "some valid op"
(reg/register! ::pure-node [:and ::node [:map [:wf/op [:= :wf.op/pure]]]])
(reg/register! ::call-node [:and ::node [:map [:wf/op [:= :wf.op/call]]]])
(reg/register! ::seq-node  [:and ::node [:map [:wf/op [:= :wf.op/seq]]]])
(reg/register! ::par-node  [:and ::node [:map [:wf/op [:= :wf.op/par]]]])
(reg/register! ::gate-node [:and ::node [:map [:wf/op [:= :wf.op/gate]]]])
(reg/register! ::loop-node [:and ::node [:map [:wf/op [:= :wf.op/loop]]]])
(reg/register! ::let-node  [:and ::node [:map [:wf/op [:= :wf.op/let]]]])
;; tuple inputs for the 2-arity constructors
(reg/register! ::gate-in [:tuple ::gate-args ::children])
(reg/register! ::loop-in [:tuple ::args      ::children])
(reg/register! ::let-in  [:tuple ::let-args  ::children])

;; --- the structural-path invariant edn->ast must establish ---
(defn ids-are-paths?
  ([n] (ids-are-paths? n []))
  ([n path]
   (and (= (:wf/id n) path)
        (every? identity
                (map-indexed (fn [i c] (ids-are-paths? c (conj path i)))
                             (:wf/children n))))))

;; uncurried adapters so the single-arg bridge can drive the 2-arity ops
(defn gate*-of [[a c]] (ast/gate* a c))
(defn loop*-of [[a c]] (ast/loop* a c))
(defn let*-of  [[a c]] (ast/let* a c))

;; relational teeth for children-wrapping constructors: op pinned + child count
;; preserved + each child's :wf/id is its structural path + args carried through
(defn- wraps? [op]
  (fn [[a c] out]
    (and (= op (:wf/op out))
         (= (count c) (count (:wf/children out)))
         (= a (:wf/args out))
         (every? identity
                 (map-indexed (fn [i child] (= [i] (:wf/id child)))
                              (:wf/children out))))))

;; --- leaf constructors: pinned op + input flows to :wf/args ---
(hst/deftrifecta-from-schema pure*-tests ast/pure* {:in ::args :out ::pure-node :num-tests 40 :contract true
                                                    :rel (fn [in out] (and (= :wf.op/pure (:wf/op out)) (= in (:wf/args out))))})
(hst/deftrifecta-from-schema call*-tests ast/call* {:in ::call-args :out ::call-node :num-tests 40 :contract true
                                                    :rel (fn [in out] (and (= :wf.op/call (:wf/op out)) (= in (:wf/args out))))})

;; --- children constructors: op + child-count + id-paths + args ---
(hst/deftrifecta-from-schema seq*-tests ast/seq* {:in ::children :out ::seq-node :num-tests 25
                                                  :rel (fn [c out] ((wraps? :wf.op/seq) [{} c] out))})
(hst/deftrifecta-from-schema par*-tests ast/par* {:in ::children :out ::par-node :num-tests 25
                                                  :rel (fn [c out] ((wraps? :wf.op/par) [{} c] out))})
(hst/deftrifecta-from-schema gate*-tests gate*-of {:in ::gate-in :out ::gate-node :num-tests 25
                                                   :rel (wraps? :wf.op/gate)})
(hst/deftrifecta-from-schema loop*-tests loop*-of {:in ::loop-in :out ::loop-node :num-tests 25
                                                   :rel (wraps? :wf.op/loop)})
(hst/deftrifecta-from-schema let*-tests  let*-of  {:in ::let-in  :out ::let-node  :num-tests 25
                                                   :rel (wraps? :wf.op/let)})

;; --- transforms over WorkflowAST ---
(hst/deftrifecta-from-schema ->edn-tests    ast/->edn    {:in ::node :out ::node :num-tests 25
                                                          :rel (fn [in out] (= in out))})       ; identity on valid input
(hst/deftrifecta-from-schema edn->ast-tests ast/edn->ast {:in ::node :out ::node :num-tests 25
                                                          :idempotent? true
                                                          :rel (fn [_in out] (ids-are-paths? out))})

;; --- predicate: accepts valid nodes, rejects schema-corrupted ones ---
(hst/deftrifecta-predicate valid?-tests ast/valid? {:schema ::node :num-tests 40})

;; --- non-vacuity guard: the recursive oracle still has teeth ---
(deftest teeth-guard-test
  (let [ok? (hst/output-oracle ::pure-node)]
    (is (ok? (ast/pure* {:v 1})))                             ; real op conforms
    (is (not (ok? (assoc (ast/pure* {:v 1}) :wf/op :wf.op/call))))  ; wrong op tag rejected (pinned)
    (is (not (ok? (dissoc (ast/pure* {:v 1}) :wf/op))))       ; dropped required key rejected
    (is (= 10 (count (hst/schema-mutants ast/pure* ::pure-node))))  ; 5 required keys -> 10 mutants
    (is (= ["drop-key-:k"] (mapv first (hst/schema-mutants identity [:map [:k :any]]))))))  ; :any -> no false wrong-type
