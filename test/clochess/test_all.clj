(ns clochess.test-all
    (:require [clojure.test :refer :all]
              [clochess.core]
              [clochess.core-api]
              [clochess.construct]))

(deftest test-all
    "Bootstrapping with the required namespaces, finds all the clochess.* namespaces (except this one),
    requires them, and runs all their tests."
    (let [namespaces (->> (all-ns)
                          (map str)
                          (filter (fn [x] (re-matches #"clochess\..*" x)))
                          (remove (fn [x] (= "clochess.test-all" x)))
                          (map symbol))]
      (is (successful? (time (apply run-tests namespaces))))))
