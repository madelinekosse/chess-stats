;; Magnus - a Clojure chess library
;; Copyright (C) 2020  Anders Eriksson

;; This file is part of Magnus.

;; Magnus is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Magnus is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with Magnus.  If not, see <https://www.gnu.org/licenses/>.

(ns chess-stats.test-all
  (:require [clojure.test :refer [deftest is run-tests successful?]]
            [chess-stats.core]
            [chess-stats.game-state.construct]
            [chess-stats.util.notation]
            [chess-stats.game-state.util]
            [chess-stats.game-state.play]
            [chess-stats.pgn]
            [chess-stats.game]
            ))

(deftest test-all
  ; Bootstrapping with the required namespaces, finds all the magnus.*
  ;   namespaces (except this one), requires them, and runs all their tests.
  (let [namespaces (->> (all-ns)
                        (map str)
                        (filter #(re-matches #"chess-stats\..*" %))
                        (remove #(= "chess-stats.test-all" %))
                        (map symbol))]
    (is (successful? (time (apply run-tests namespaces))))))
