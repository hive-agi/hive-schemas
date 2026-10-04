(ns hive-schemas.git-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-schemas.git :as git]
            [hive-spi.schema.registry :as reg]
            [malli.core :as m]
            [malli.generator :as mg]
            [hive-schemas.test :as hst]))

(def schemas
  {:area git/Area :area-ref git/AreaRef :line-range git/LineRange
   :line-op git/LineOp :line git/Line :hunk git/Hunk
   :change-status git/ChangeStatus :file-change git/FileChange
   :area-diff git/AreaDiff :blob git/Blob :conflict-kind git/ConflictKind
   :conflict-region git/ConflictRegion :conflict git/Conflict
   :stash git/Stash :operation git/Operation :status git/Status})

(deftest registered-generator-roundtrips
  (doseq [[name schema] schemas]
    (testing (str name)
      (let [id (keyword "hive.schemas.git" (clojure.core/name name))]
        (is (some? (reg/schema id)))
        (doseq [value (mg/sample schema {:size 12 :seed 42})]
          (is (m/validate schema value)))))))

(defn identity-area-ref [x] x)

(hst/deftrifecta-from-schema area-ref-trifecta hive-schemas.git-test/identity-area-ref
  {:in :hive.schemas.git/area-ref :out :hive.schemas.git/area-ref
   :mutation false :num-tests 25})