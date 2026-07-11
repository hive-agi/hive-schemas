(ns hive-schemas.hive-spi-role-test
  "Bridge v2 fanned out over hive-spi.role: the RoleCard validator in predicate
   mode. Positive — every generated RoleCard is accepted; negative — dropping a
   required key (:role/id / :role/name) or wrong-typing it is rejected. RoleCard
   is an OPEN map, so the teeth come strictly from the two required keys."
  (:require [clojure.test :refer [deftest is]]
            [hive-spi.role.card :as role]
            [hive-spi.schema.registry :as reg]
            [hive-schemas.test :as hst]))

(reg/register! ::role-card role/RoleCard)

(hst/deftrifecta-predicate role-valid?-tests hive-spi.role.card/valid?
  {:schema ::role-card :num-tests 40})

;; --- required-key teeth on an open map ---
(deftest role-teeth-guard-test
  (let [good {:role/id :role/reviewer :role/name "Reviewer"}]
    (is (role/valid? good))
    (is (not (role/valid? (dissoc good :role/id))))               ; required :role/id
    (is (not (role/valid? (assoc good :role/name :not-a-string)))) ; wrong-type :role/name
    (is (= [:role/id :role/name] (mapv first (hst/required-entries ::role-card))))))
