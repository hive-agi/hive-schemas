(ns hive-schemas.skeleton
  "Test-namespace skeletons generated from a source namespace's contract
   inventory, read from the file WITHOUT loading it (see `hive-schemas.census`).

   For one source file the skeleton holds:
     - one `deftrifecta-from-schema` per arity of every PURE contracted public fn,
       `:in` and `:out` lifted from its `m/=>` (or `:malli/schema`) form with
       every symbol qualified, so the test compiles without the source's aliases;
     - one `deftest-contract-coverage` gate over the file, whose `:exempt` lists
       each uncontracted public fn with a BLANK reason, so the gate fails until
       the fn is contracted or the exemption is argued;
     - the effectful contracted fns (name ending in `!`, and `-main`), which need
       a port stub rather than a trifecta.

   What is left to the author: `:rel` oracles, goldens and port stubs.

   Levers:
     schema-arities  fn schema form -> [{:in form :out form :arity n}]
     inventory       file forms -> {:ns :alias :contracts :uncontracted :defs ...}
     qualify         schema form, inventory -> the form with symbols qualified
     skeleton        inventory opts -> the skeleton as data
     render          skeleton -> the test namespace source text
     skeleton-of     file opts -> the skeleton, read from disk

   CLI: clojure -M -m hive-schemas.skeleton [--paths P,...] FILE ...
        prints one test namespace per source file."
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [hive-schemas.census :as census]
            [malli.core :as m]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

;; =============================================================================
;; Promote: schema forms
;; =============================================================================

(defn- arrow-arity [form]
  (when (and (vector? form) (= :=> (first form)) (vector? (second form)))
    (let [[_ in out] form]
      {:in in :out out :arity (if (= :cat (first in)) (count (rest in)) 1)})))

(defn schema-arities
  "The `[:=> in out]` arities of a function schema FORM, one map per arity:
   `{:in in :out out :arity n}`. `[:function ...]` yields every child arity.
   Empty when the form is not a literal function schema."
  [form]
  (cond
    (and (vector? form) (= :function (first form))) (into [] (keep arrow-arity) (rest form))
    (arrow-arity form) [(arrow-arity form)]
    :else []))

(defn effectful?
  "True for a fn name the skeleton gives no trifecta: `-main`, or a name ending in `!`."
  [fn-name]
  (or (= "-main" fn-name) (str/ends-with? fn-name "!")))

(defn- last-segment [ns-name]
  (subs ns-name (inc (or (str/last-index-of ns-name ".") -1))))

;; =============================================================================
;; Collect: the inventory of one file's forms
;; =============================================================================

(defn- ns-form [forms]
  (first (filter #(and (seq? %) (= 'ns (first %)) (symbol? (second %))) forms)))

(defn- arrow? [form aliases]
  (and (seq? form) (symbol? (first form)) (symbol? (second form))
       (= "=>" (name (first form)))
       (some-> (namespace (first form)) (as-> q (get aliases q q)) (= "malli.core"))))

(defn- top-defs [forms]
  (into {}
        (keep (fn [f]
                (when (and (seq? f) (= 'def (first f)) (symbol? (second f)))
                  [(str (second f)) (boolean (:private (meta (second f))))])))
        forms))

(defn- meta-schemas [forms]
  (into {}
        (keep (fn [f]
                (when (and (seq? f) (contains? #{'defn 'defn-} (first f)) (symbol? (second f)))
                  (let [n (second f) attr (nth f 2 nil)
                        s (or (:malli/schema (meta n)) (when (map? attr) (:malli/schema attr)))]
                    (when s [(str n) s])))))
        forms))

(defn inventory
  "The contract inventory of one source file's top-level `forms`:
   `{:ns :alias :aliases :defs {name private?} :contracts {fn-name schema-form}
     :uncontracted #{fn-name}}` over its PUBLIC defns. nil without an ns form."
  [forms]
  (when-let [nsf (ns-form forms)]
    (let [{:keys [aliases]} (census/ns-requires nsf)
          {:keys [ns defns]} (census/source-summary forms)
          arrows    (into {}
                          (keep (fn [f]
                                  (when (and (arrow? f aliases) (nil? (namespace (second f))))
                                    [(str (second f)) (nth f 2 nil)])))
                          forms)
          contracts (into (sorted-map)
                          (filter (fn [[k _]] (contains? defns k)))
                          (merge (meta-schemas forms) arrows))]
      {:ns           ns
       :alias        (last-segment ns)
       :aliases      aliases
       :defs         (top-defs forms)
       :contracts    contracts
       :uncontracted (into (sorted-set) (remove #(contains? contracts %)) defns)})))

(defn qualify
  "`form` with every symbol a test namespace could not resolve qualified:
   an alias-qualified symbol gets the aliased namespace, a bare symbol naming a
   top-level def of the source gets the source namespace, and a PRIVATE def is
   reached through its var. Other symbols (clojure.core) are left alone."
  [form {:keys [ns aliases defs]}]
  (walk/postwalk
   (fn [x]
     (cond
       (not (symbol? x)) x
       (namespace x)     (if-let [full (get aliases (namespace x))] (symbol full (name x)) x)
       (contains? defs (name x))
       (if (get defs (name x))
         (list 'deref (list 'var (symbol ns (name x))))
         (symbol ns (name x)))
       :else x))
   form))

(defn- referenced-nses [form]
  (into (sorted-set)
        (keep #(when (and (symbol? %) (namespace %)) (namespace %)))
        (tree-seq coll? seq form)))

;; =============================================================================
;; Promote: the skeleton as data
;; =============================================================================

(defn skeleton
  "The test-namespace skeleton for `inv` (an `inventory`) as data:
   `{:test-ns :requires :trifectas :gate :effectful}`.

   opts: :paths  the gate's source paths, relative to the repo root   [required]
         :exempt-reason  the reason written for an uncontracted fn  (default \"\")"
  [inv {:keys [paths exempt-reason] :or {exempt-reason ""}}]
  (let [{:keys [ns alias contracts uncontracted]} inv
        pure      (remove (comp effectful? key) contracts)
        trifectas (vec (for [[fn-name form] pure
                             :let [arities (schema-arities form)]
                             {:keys [in out arity]} arities]
                         {:name    (symbol (if (= 1 (count arities)) fn-name (str fn-name "-arity" arity)))
                          :subject (symbol ns fn-name)
                          :in      (qualify in inv)
                          :out     (qualify out inv)}))
        schemas   (mapcat (juxt :in :out) trifectas)]
    {:test-ns   (symbol (str ns "-test"))
     :requires  (into [['clojure.test :refer ['deftest 'is]]
                       ['hive-schemas.coverage :as 'coverage]
                       ['hive-schemas.test :refer ['deftrifecta-from-schema]]
                       [(symbol ns) :as (symbol alias)]]
                      (comp (remove #{ns "clojure.core" "hive-schemas.coverage" "hive-schemas.test"})
                            (map (fn [n] [(symbol n)])))
                      (into (sorted-set) (mapcat referenced-nses) schemas))
     :trifectas trifectas
     :gate      {:name   (symbol (str alias "-contracts"))
                 :paths  (vec paths)
                 :exempt (into (sorted-map)
                               (map (fn [n] [(symbol ns n) exempt-reason]))
                               uncontracted)}
     :effectful (into (sorted-set) (filter effectful?) (keys contracts))}))

;; =============================================================================
;; Pipeline: rendering
;; =============================================================================

(defn- code-str [form]
  (binding [*print-namespace-maps* false *print-meta* false
            *print-length* nil *print-level* nil pp/*print-right-margin* 92]
    (str/trimr (with-out-str (pp/write form :dispatch pp/code-dispatch)))))

(defn render
  "The source text of the test namespace `skel` (a `skeleton`) describes."
  [{:keys [test-ns requires trifectas gate effectful]}]
  (str/join
   "\n\n"
   (concat
    [(code-str (list 'ns test-ns (apply list :require requires)))]
    (for [{:keys [name subject in out]} trifectas]
      (code-str (list 'deftrifecta-from-schema name subject {:in in :out out})))
    (when (seq effectful)
      [(str ";; Effectful, needs a port stub instead of a trifecta: "
            (str/join ", " effectful))])
    [(str (code-str (list 'coverage/deftest-contract-coverage (:name gate)
                          {:paths (:paths gate) :exempt (:exempt gate)}))
          "\n")])))

;; =============================================================================
;; Boundary: files and CLI
;; =============================================================================

(defn skeleton-of
  "The skeleton of the source `file`, or nil when it does not parse or has no
   ns form. opts are `skeleton`'s; :paths defaults to [file]."
  [file opts]
  (some-> (census/read-forms file)
          inventory
          (skeleton (merge {:paths [(str file)]} opts))))

(defn parse-args
  "`{:files [path ...] :paths [path ...]?}` from CLI args; --paths takes one
   comma-separated value."
  [args]
  (loop [[a & more] args acc {:files []}]
    (cond
      (nil? a)          acc
      (= "--paths" a)   (recur (rest more)
                               (assoc acc :paths (into [] (remove str/blank?)
                                                       (str/split-lines (str/replace (str (first more)) "," "\n")))))
      :else             (recur more (update acc :files conj a)))))

(defn -main
  "Print the test-namespace skeleton of each source file argument."
  [& args]
  (let [{:keys [files paths]} (parse-args args)]
    (doseq [f files]
      (if-let [skel (skeleton-of f (cond-> {} paths (assoc :paths paths)))]
        (println (render skel))
        (binding [*out* *err*] (println "skeleton: no ns form or unreadable:" f))))))

;; =============================================================================
;; Contracts
;; =============================================================================

(def Arity
  "One arity of a function schema form."
  [:map [:in :any] [:out :any] [:arity nat-int?]])

(def Inventory
  "A source file's contract inventory."
  [:map
   [:ns :string] [:alias :string]
   [:aliases [:map-of :string :string]]
   [:defs [:map-of :string :boolean]]
   [:contracts [:map-of :string :any]]
   [:uncontracted [:set :string]]])

(def Skeleton
  "A test-namespace skeleton."
  [:map
   [:test-ns :symbol]
   [:requires [:vector [:vector :any]]]
   [:trifectas [:vector [:map [:name :symbol] [:subject :symbol] [:in :any] [:out :any]]]]
   [:gate [:map [:name :symbol] [:paths [:vector :string]] [:exempt [:map-of :symbol :string]]]]
   [:effectful [:set :string]]])

(m/=> schema-arities [:=> [:cat :any] [:vector Arity]])
(m/=> effectful? [:=> [:cat :string] :boolean])
(m/=> inventory [:=> [:cat [:maybe [:sequential :any]]] [:maybe Inventory]])
(m/=> qualify [:=> [:cat :any Inventory] :any])
(m/=> skeleton [:=> [:cat Inventory [:map [:paths [:sequential :string]]]] Skeleton])
(m/=> render [:=> [:cat Skeleton] :string])
(m/=> skeleton-of [:=> [:cat :any [:maybe :map]] [:maybe Skeleton]])
(m/=> parse-args [:=> [:cat [:sequential :string]] [:map [:files [:vector :string]]]])
(m/=> -main [:=> [:cat [:* :string]] :nil])
