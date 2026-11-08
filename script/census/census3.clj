;; Static fleet census v3: malli contract coverage + hive-test facet coverage.
;; bb census3.clj <root> [repo ...]          table, same columns as census2
;; bb census3.clj --edn <root> repo          per-ns detail as EDN (for diffing)
;; v3 2026-10-31 (card 20261014203048-078fa409): READS forms with edamame instead
;; of regexes, so it sees what census2 missed:
;;  (a) defn metadata in any shape (^:private, ^{...} over several lines, defn-),
;;  (b) defns and m/=> inside #?(:clj ...) / #?@(:clj [...]) (read under :clj,
;;      the same feature hive-schemas.coverage/file-defns reads with),
;;  (c) contracts as m/=> under ANY alias of malli.core or hive-test.contract
;;      (resolved from the ns form, :refer [=>] included), {:malli/schema ..}
;;      in the attr map or metadata, and mx/defn (malli.experimental) with :-
;;      annotations,
;;  (d) a test ns credits a source ns only when it REQUIRES it (parsed from the
;;      ns form) or is named exactly <ns>-test; census2's prefix match also
;;      credited foo.bar from foo.bar-baz-test.
;; census2.clj is kept unchanged so older numbers stay reproducible.
(require '[clojure.java.io :as io] '[clojure.string :as str] '[edamame.core :as e])

(def args *command-line-args*)
(def edn? (= "--edn" (first args)))
(def args* (if edn? (rest args) args))
(def root (io/file (or (first args*) ".")))
(def only (set (rest args*)))

(defn files [dir exts]
  (when (.isDirectory (io/file dir))
    (->> (file-seq (io/file dir))
         (filter #(.isFile %))
         (filter #(some (fn [x] (str/ends-with? (.getName %) x)) exts))
         (remove #(str/includes? (.getPath %) "/resources/"))
         (remove #(some (fn [seg] (or (str/starts-with? seg ".") (#{"target" "node_modules"} seg)))
                        (str/split (.getPath %) #"/"))))))

(def parse-opts
  {:all true :read-cond :allow :features #{:clj}
   :auto-resolve (fn [_] 'census.unresolved)
   :readers (fn [_] identity)
   :regex true})

(defn read-forms
  "Top-level forms of file f, splicing #?@ and unwrapping (do ...).
  nil when the file does not parse."
  [f]
  (try (let [fs (e/parse-string-all (slurp f) parse-opts)]
         (mapcat (fn flat [x] (if (and (seq? x) (= 'do (first x))) (mapcat flat (rest x)) [x])) fs))
       (catch Exception _ nil)))

(defn ns-form [forms] (first (filter #(and (seq? %) (= 'ns (first %)) (symbol? (second %))) forms)))

(defn requires
  "{alias-or-ns-string ns-string} and the set of required ns strings of an ns form."
  [nsf]
  (let [specs (->> (rest nsf)
                   (filter #(and (seq? %) (#{:require :require-macros} (first %))))
                   (mapcat rest))
        expand (fn expand [prefix s]
                 (cond (symbol? s) [[(str prefix s) nil]]
                       (and (sequential? s) (symbol? (first s)))
                       (let [n (str prefix (first s)) opts (rest s)]
                         (if (and (seq opts) (sequential? (first opts)))
                           (mapcat #(expand (str n ".") %) opts)
                           (let [m (apply hash-map (if (even? (count opts)) opts (butlast opts)))]
                             [[n (some-> (:as m) str) (when (sequential? (:refer m)) (map str (:refer m)))]])))
                       :else []))
        pairs (mapcat #(expand "" %) specs)]
    {:aliases (into {} (keep (fn [[n a]] (when a [a n]))) pairs)
     :refers (into {} (for [[n _ rs] pairs r rs] [r n]))
     :required (set (map first pairs))}))

(def malli-nss #{"malli.core" "hive-test.contract"})
(def mx-nss #{"malli.experimental"})

(defn head-in? [form aliases nss local-names]
  (when (and (seq? form) (symbol? (first form)))
    (let [h (first form) q (namespace h)]
      (if q (contains? nss (get aliases q q)) (contains? local-names (name h))))))

(defn defn-name+private [form]
  (when (and (seq? form) ('#{defn defn-} (first form)) (symbol? (second form)))
    (let [n (second form) attr (nth form 2 nil)]
      {:name (str n)
       :private? (or (= 'defn- (first form)) (:private (meta n)) (and (map? attr) (:private attr)))
       :schema-meta? (or (contains? (meta n) :malli/schema) (and (map? attr) (contains? attr :malli/schema)))})))

(defn analyse-src [f]
  (when-let [forms (read-forms f)]
    (when-let [nsf (ns-form forms)]
      (let [ns (str (second nsf))
            {:keys [aliases refers required]} (requires nsf)
            referred (if (contains? malli-nss (get refers "=>")) #{"=>"} #{})
            d (keep defn-name+private forms)
            mx (for [x forms :when (and (head-in? x aliases mx-nss #{}) (= "defn" (name (first x))) (symbol? (second x)))]
                 {:name (str (second x)) :private? (:private (meta (second x)))
                  :schema-meta? (boolean (some #{:-} (flatten (map #(if (coll? %) (seq %) [%]) (drop 2 x)))))})
            arrows (for [x forms :when (and (head-in? x aliases malli-nss referred) (= "=>" (name (first x))) (symbol? (second x)))]
                     (second x))
            local (set (for [s arrows :when (nil? (namespace s))] (str s)))
            remote (set (for [s arrows :when (namespace s)] (str (get aliases (namespace s) (namespace s)) "/" (name s))))
            pub (remove :private? (concat d mx))]
        {:ns ns :required required
         :defns (set (map :name pub))
         :local-contracts (into local (keep #(when (:schema-meta? %) (:name %))) pub)
         :remote-contracts remote}))))

(def facet-heads
  "Facet -> name prefixes of the hive-test macros that carry it (deftrifecta-from-multi,
  deftest-golden-fn, deftest-mutations, deftest-mutation-witness ... all count)."
  {:trifecta ["deftrifecta"]
   :golden   ["deftest-golden"]
   :prop     ["defprop" "defspec" "defcheck"]
   :mutation ["deftest-mutation" "with-mutation"]})

(defn facets-of [forms]
  (let [heads (set (for [x (tree-seq coll? seq forms) :when (and (seq? x) (symbol? (first x)))] (name (first x))))]
    (into #{} (for [[k ps] facet-heads :when (some (fn [h] (some #(str/starts-with? h %) ps)) heads)] k))))

(defn repo-stats [repo]
  (let [srcs (keep analyse-src (files (io/file repo "src") [".clj" ".cljc"]))
        spine (into #{} (mapcat :remote-contracts) srcs)
        tests (for [f (files (io/file repo "test") [".clj" ".cljc"])
                    :let [forms (read-forms f) nsf (some-> forms ns-form)] :when nsf]
                {:ns (str (second nsf)) :required (:required (requires nsf)) :facets (facets-of forms)})
        faceted (filterv #(seq (:facets %)) tests)
        per (vec (for [s srcs :when (seq (:defns s))
                       :let [ts (filter #(or (= (:ns %) (str (:ns s) "-test")) ((:required %) (:ns s))) faceted)
                             fs (into #{} (mapcat :facets) ts)]]
                   (merge {:ns (:ns s) :defns (:defns s)
                           :covered (set (filter #(or ((:local-contracts s) %) (spine (str (:ns s) "/" %))) (:defns s)))}
                          (into {} (for [k (keys facet-heads)] [k (contains? fs k)])))))
        cnt (fn [k] (count (filter k per)))]
    {:repo (.getName repo) :per per
     :fns (reduce + (map (comp count :defns) per))
     :contracted (reduce + (map (comp count :covered) per))
     :ns (count per)
     :trifecta (cnt :trifecta) :golden (cnt :golden) :prop (cnt :prop) :mutation (cnt :mutation)
     :hive-tested (count (filter #(some % (keys facet-heads)) per))}))

(def repos
  (->> (.listFiles root) (filter #(.isDirectory %))
       (filter #(.exists (io/file % "deps.edn")))
       (remove #(re-find #"(-wt|\.git-backup|^ref-|^clones|^labs|^scratch|^hm-census|-renovate|^presentation|^case-studies|-progress-cost)" (.getName %)))
       (filter #(or (empty? only) (only (.getName %))))
       (sort-by #(.getName %))))

(if edn?
  (prn (into {} (for [r repos :let [s (repo-stats r)]]
                  [(:repo s) (into {} (for [p (:per s)] [(:ns p) (dissoc p :ns)]))])))
  (let [rows (->> repos (map repo-stats) (filter #(pos? (:fns %))))
        pct (fn [a b] (if (pos? b) (format "%5.1f" (* 100.0 (/ a b))) "  -  "))
        line (fn [n r] (format "%-28s %6d %6s %4d %6s %6s %6s %6s %6s" n (:fns r) (pct (:contracted r) (:fns r)) (:ns r)
                               (pct (:hive-tested r) (:ns r)) (pct (:trifecta r) (:ns r)) (pct (:golden r) (:ns r)) (pct (:prop r) (:ns r)) (pct (:mutation r) (:ns r))))]
    (println (format "%-28s %6s %6s %4s %6s %6s %6s %6s %6s" "repo" "fns" "schm%" "ns" "hive%" "trif%" "gold%" "prop%" "mut%"))
    (doseq [r (sort-by :fns > rows)] (println (line (:repo r) r)))
    (println (line "TOTAL" (into {} (for [k [:fns :contracted :ns :hive-tested :trifecta :golden :prop :mutation]]
                                      [k (reduce + (map k rows))]))))))
