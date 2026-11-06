(ns hive-schemas.census
  "Static fleet census: malli contract coverage and hive-test facet coverage per
   repo, read from the source files WITHOUT loading them.

   `hive-schemas.coverage` answers the same contract question from the live
   registry, inside one JVM, for one repo. The census answers it for many repos
   at once and for repos that will not load together, at the price of seeing
   only contracts written as forms: `m/=>` under any alias of malli.core,
   `:malli/schema` in a defn's metadata or attr map, and malli.experimental
   `defn` with `:-` annotations.

   A test namespace credits a source namespace when it REQUIRES it or is named
   exactly `<ns>-test`; the facets it credits are the hive-test macro families
   in `facet-heads`.

   Levers:
     read-forms        file  -> top-level forms | nil when it does not parse
     ns-requires       ns form -> {:aliases {alias ns} :required #{ns}}
     source-summary    forms -> {:ns :defns :local-contracts :remote-contracts}
     test-summary      forms -> {:ns :required :facets}
     repo-census       repo dir -> one census row (with :per namespace detail)
     floors            rows  -> {repo {:contract-pct :facet-pct}}
     ratchet-failures  rows floors -> messages for every floor a row fell below
     deftest-census-ratchet  a clojure.test gate over `ratchet-failures`

   CLI: clojure -M -m hive-schemas.census [--detail] [--ratchet FILE]
        [--record FILE] [repo-dir ...]
   prints one EDN row per repo; --ratchet exits 1 when a floor is broken;
   --record writes the current percentages as the new floors."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.test]
            [clojure.tools.reader :as tr]
            [clojure.tools.reader.reader-types :as rt]
            [malli.core :as m]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def facet-heads
  "Facet -> name prefixes of the hive-test macros that carry it."
  {:trifecta ["deftrifecta"]
   :golden   ["deftest-golden"]
   :prop     ["defprop" "defspec" "defcheck"]
   :mutation ["deftest-mutation" "with-mutation"]})

(def ^:private skipped-segments #{"target" "node_modules" "resources"})

;; =============================================================================
;; Reading
;; =============================================================================

(defn- census-file? [^java.io.File f]
  (and (.isFile f)
       (let [n (.getName f)] (or (str/ends-with? n ".clj") (str/ends-with? n ".cljc")))
       (not-any? #(or (str/starts-with? % ".") (contains? skipped-segments %))
                 (map str (.toPath f)))))

(defn- census-files [dir]
  (let [d (io/file dir)]
    (if (.isDirectory d)
      (sort-by #(.getPath ^java.io.File %) (filter census-file? (file-seq d)))
      [])))

(defn- flatten-do [x]
  (if (and (seq? x) (= 'do (first x))) (mapcat flatten-do (rest x)) [x]))

(def ^:private any-alias
  "The alias resolver: every alias resolves to one placeholder. The JVM reader
   calls `*alias-map*`; babashka's derefs it first, hence the delay there."
  (let [f (fn [_] 'hive-schemas.census.unresolved-alias)]
    (if (System/getProperty "babashka.version") (delay f) f)))

(defn read-forms
  "Top-level forms of `file` read under the :clj feature, `(do ...)` unwrapped and
   `#?@` spliced (the text is read as one vector, where splicing is legal).
   nil when the file does not parse. Aliases and unknown tags resolve to
   placeholders: only the shape of ns, defn and m/=> forms is inspected.
   `*read-eval*` is off wherever the reader has it (babashka's does not)."
  [file]
  (try
    (with-bindings (cond-> {#'tr/*alias-map* any-alias
                            #'tr/*default-data-reader-fn* (fn [_ v] v)}
                     (resolve 'clojure.tools.reader/*read-eval*)
                     (assoc (resolve 'clojure.tools.reader/*read-eval*) false))
      (let [rdr (rt/string-push-back-reader (str "[" (slurp file) "\n]"))]
        (vec (mapcat flatten-do (tr/read {:read-cond :allow :features #{:clj}} rdr)))))
    (catch Exception _ nil)))

(defn- ns-form [forms]
  (first (filter #(and (seq? %) (= 'ns (first %)) (symbol? (second %))) forms)))

(defn- expand-spec [prefix spec]
  (cond
    (symbol? spec) [[(str prefix spec) nil]]
    (and (sequential? spec) (symbol? (first spec)))
    (let [n (str prefix (first spec)) opts (rest spec)]
      (if (and (seq opts) (or (sequential? (first opts)) (symbol? (first opts))))
        (mapcat #(expand-spec (str n ".") %) opts)
        [[n (some (fn [[k v]] (when (contains? #{:as :as-alias} k) (str v)))
                  (partition 2 1 opts))]]))
    :else []))

(defn ns-requires
  "`{:aliases {alias-string ns-string} :required #{ns-string}}` of an ns form."
  [ns-form]
  (let [pairs (->> (rest ns-form)
                   (filter #(and (seq? %) (contains? #{:require :require-macros} (first %))))
                   (mapcat rest)
                   (mapcat #(expand-spec "" %)))]
    {:aliases  (into {} (keep (fn [[n a]] (when a [a n]))) pairs)
     :required (into (sorted-set) (map first) pairs)}))

;; =============================================================================
;; Summaries
;; =============================================================================

(defn- head-from? [form aliases ns-name head-name]
  (and (seq? form) (symbol? (first form)) (symbol? (second form))
       (= head-name (name (first form)))
       (some-> (namespace (first form)) (as-> q (get aliases q q)) (= ns-name))))

(defn- plain-defn [form]
  (when (and (seq? form) (contains? #{'defn 'defn-} (first form)) (symbol? (second form)))
    (let [n (second form) attr (nth form 2 nil)]
      {:name     (str n)
       :private? (boolean (or (= 'defn- (first form)) (:private (meta n))
                              (and (map? attr) (:private attr))))
       :schema?  (boolean (or (contains? (meta n) :malli/schema)
                              (and (map? attr) (contains? attr :malli/schema))))})))

(defn source-summary
  "What a source file's forms declare: its ns, its public defns, the names it
   contracts itself and the `ns/name` strings it contracts elsewhere. nil when
   the forms carry no ns form."
  [forms]
  (when-let [nsf (ns-form forms)]
    (let [{:keys [aliases required]} (ns-requires nsf)
          mx     (for [x forms :when (head-from? x aliases "malli.experimental" "defn")]
                   {:name     (str (second x))
                    :private? (boolean (:private (meta (second x))))
                    :schema?  (boolean (some #{:-} (tree-seq coll? seq (drop 2 x))))})
          arrows (for [x forms :when (head-from? x aliases "malli.core" "=>")] (second x))
          pub    (remove :private? (concat (keep plain-defn forms) mx))]
      {:ns               (str (second nsf))
       :required         required
       :defns            (into (sorted-set) (map :name) pub)
       :local-contracts  (into (into (sorted-set) (keep #(when-not (namespace %) (str %))) arrows)
                               (keep #(when (:schema? %) (:name %))) pub)
       :remote-contracts (into (sorted-set)
                               (keep #(when-let [q (namespace %)]
                                        (str (get aliases q q) "/" (name %))))
                               arrows)})))

(defn facets-of
  "The facets whose hive-test macro heads occur anywhere in `forms`."
  [forms]
  (let [heads (into #{} (keep #(when (and (seq? %) (symbol? (first %))) (name (first %))))
                    (tree-seq coll? seq forms))]
    (into (sorted-set)
          (keep (fn [[k prefixes]]
                  (when (some (fn [h] (some #(str/starts-with? h %) prefixes)) heads) k)))
          facet-heads)))

(defn test-summary
  "A test file's ns, the namespaces it requires and the facets it carries; nil
   without an ns form."
  [forms]
  (when-let [nsf (ns-form forms)]
    {:ns (str (second nsf)) :required (:required (ns-requires nsf)) :facets (facets-of forms)}))

;; =============================================================================
;; Rows
;; =============================================================================

(defn percent
  "100*a/b rounded to one decimal; 0.0 when b is zero."
  [a b]
  (if (pos? b) (/ (Math/round (/ (* 1000.0 a) b)) 10.0) 0.0))

(defn repo-census
  "One census row for the repo at `repo`: counts, `:contract-pct` (contracted
   public fns over public fns), `:facet-pct` (namespaces credited with at least
   one facet over namespaces with public fns), `:unreadable` files and the
   `:per` namespace detail.

   opts: :name (default the directory name), :src-paths [\"src\"], :test-paths [\"test\"]."
  ([repo] (repo-census repo {}))
  ([repo {:keys [name src-paths test-paths] :or {src-paths ["src"] test-paths ["test"]}}]
   (let [root    (.getCanonicalFile (io/file repo))
         read-in (fn [paths] (for [p paths f (census-files (io/file root p))]
                               [(.getPath ^java.io.File f) (read-forms f)]))
         src     (read-in src-paths)
         tst     (read-in test-paths)
         srcs    (keep (comp source-summary second) src)
         spine   (into #{} (mapcat :remote-contracts) srcs)
         faceted (filter #(seq (:facets %)) (keep (comp test-summary second) tst))
         per     (vec (for [s srcs :when (seq (:defns s))
                            :let [ts (filter #(or (= (:ns %) (str (:ns s) "-test"))
                                                  (contains? (:required %) (:ns s)))
                                             faceted)]]
                        {:ns      (:ns s)
                         :defns   (:defns s)
                         :covered (into (sorted-set)
                                        (filter #(or (contains? (:local-contracts s) %)
                                                     (contains? spine (str (:ns s) "/" %))))
                                        (:defns s))
                         :facets  (into (sorted-set) (mapcat :facets) ts)}))
         fns     (reduce + (map (comp count :defns) per))
         covered (reduce + (map (comp count :covered) per))
         tested  (count (filter (comp seq :facets) per))
         with    (fn [k] (count (filter #(contains? (:facets %) k) per)))]
     (merge {:repo         (or name (.getName root))
             :fns          fns
             :contracted   covered
             :ns           (count per)
             :hive-tested  tested
             :contract-pct (percent covered fns)
             :facet-pct    (percent tested (count per))
             :unreadable   (into [] (keep (fn [[p forms]] (when (nil? forms) p))) (concat src tst))
             :per          per}
            (into {} (map (juxt identity with)) (keys facet-heads))))))

;; =============================================================================
;; Ratchet
;; =============================================================================

(defn floors
  "The floors a set of rows records: `{repo {:contract-pct x :facet-pct y}}`."
  [rows]
  (into (sorted-map) (map (juxt :repo #(select-keys % [:contract-pct :facet-pct]))) rows))

(defn ratchet-failures
  "One message per floor in `floors` that its row fell below, or that has no
   row at all. Empty when every recorded floor holds."
  [rows floors]
  (let [by-repo (into {} (map (juxt :repo identity)) rows)]
    (vec (for [[repo floor] (sort-by key floors)
               [k label]    [[:contract-pct "contract%"] [:facet-pct "facet%"]]
               :let [need (get floor k) row (get by-repo repo)]
               :when (and need (or (nil? row) (< (get row k 0.0) need)))]
           (if row
             (format "%s %s %.1f fell below its floor %.1f" repo label (double (get row k)) (double need))
             (format "%s has a %s floor %.1f but was not measured" repo label (double need)))))))

(defmacro deftest-census-ratchet
  "Emit `name`, a test that censuses the repo at `:repo` (default \".\") and fails
   when its contract% or facet% drops below the floor recorded for its name in
   the EDN file `:floors-file`. Other opts are `repo-census`'s."
  [name {:keys [repo floors-file] :or {repo "."} :as opts}]
  `(clojure.test/deftest ~name
     (let [row#      (repo-census ~repo ~(dissoc opts :repo :floors-file))
           floors#   (edn/read-string (slurp ~floors-file))
           failures# (ratchet-failures [row#] (select-keys floors# [(:repo row#)]))]
       (clojure.test/is (contains? floors# (:repo row#))
                        (str "no floor recorded for " (:repo row#) " in " ~floors-file))
       (doseq [f# failures#] (clojure.test/is false f#)))))

;; =============================================================================
;; CLI
;; =============================================================================

(defn parse-args
  "`{:repos [dir ...] :detail? bool :ratchet file? :record file?}` from CLI args."
  [args]
  (loop [[a & more] args acc {:repos [] :detail? false}]
    (cond
      (nil? a)            (update acc :repos #(if (seq %) % ["."]))
      (= "--detail" a)    (recur more (assoc acc :detail? true))
      (= "--ratchet" a)   (recur (rest more) (assoc acc :ratchet (first more)))
      (= "--record" a)    (recur (rest more) (assoc acc :record (first more)))
      :else               (recur more (update acc :repos conj a)))))

(defn -main
  "Print one EDN census row per repo; see the namespace doc for the flags."
  [& args]
  (let [{:keys [repos detail? ratchet record]} (parse-args args)
        rows (mapv repo-census repos)]
    (doseq [r rows] (prn (if detail? r (dissoc r :per))))
    (when record
      (spit record (with-out-str (pp/pprint (floors rows)))))
    (when ratchet
      (let [failures (ratchet-failures rows (edn/read-string (slurp ratchet)))]
        (doseq [f failures] (binding [*out* *err*] (println "RATCHET:" f)))
        (when (seq failures) (System/exit 1))))))

;; =============================================================================
;; Contracts
;; =============================================================================

(def CensusRow
  "The counted part of a census row."
  [:map
   [:repo :string]
   [:fns nat-int?] [:contracted nat-int?] [:ns nat-int?] [:hive-tested nat-int?]
   [:contract-pct [:double {:min 0.0 :max 100.0}]]
   [:facet-pct [:double {:min 0.0 :max 100.0}]]])

(def Floors
  "Recorded floors by repo name."
  [:map-of :string [:map [:contract-pct {:optional true} number?]
                    [:facet-pct {:optional true} number?]]])

(m/=> read-forms [:=> [:cat :any] [:maybe [:vector :any]]])
(m/=> ns-requires [:=> [:cat [:sequential :any]]
                   [:map [:aliases [:map-of :string :string]] [:required [:set :string]]]])
(m/=> source-summary [:=> [:cat [:maybe [:sequential :any]]] [:maybe [:map [:ns :string]]]])
(m/=> facets-of [:=> [:cat :any] [:set (into [:enum] (keys facet-heads))]])
(m/=> test-summary [:=> [:cat [:maybe [:sequential :any]]] [:maybe [:map [:ns :string]]]])
(m/=> percent [:=> [:cat nat-int? nat-int?] :double])
(m/=> repo-census [:function [:=> [:cat :any] CensusRow] [:=> [:cat :any [:maybe :map]] CensusRow]])
(m/=> floors [:=> [:cat [:sequential CensusRow]] Floors])
(m/=> ratchet-failures [:=> [:cat [:sequential CensusRow] Floors] [:vector :string]])
(m/=> parse-args [:=> [:cat [:sequential :string]]
                  [:map [:repos [:vector :string]] [:detail? :boolean]]])
(m/=> -main [:=> [:cat [:* :string]] :nil])
