(ns clochess.test-all
  (:require [clojure.test :refer [deftest is run-tests successful?]]
            [clochess.core]
            [clochess.construct]))

(deftest test-all
  "Bootstrapping with the required namespaces, finds all the clochess.* namespaces (except this one),
    requires them, and runs all their tests."
  (let [namespaces (->> (all-ns)
                        (map str)
                        (filter #(re-matches #"clochess\..*" %))
                        (remove #(= "clochess.test-all" %))
                        (map symbol))]
    (is (successful? (time (apply run-tests namespaces))))))
