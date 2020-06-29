;; CloChess - a Clojure chess library
;; Copyright (C) 2020  Anders Eriksson

;; This file is part of CloChess.

;; CloChess is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; CloChess is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with CloChess.  If not, see <https://www.gnu.org/licenses/>.

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
