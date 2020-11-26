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

(ns chess-stats.game-state.util
  "Utility functions for Magnus. Probably not of interest unless you're
   modifying the library itself."
  (:require [clojure.test :refer [is]]))

(defn in?
  "Check if value is in collection."
  {:test (fn []
           (is (in? 1 [1 2 3]))
           (is (not (in? 4 [1 2 3]))))}
  [x coll]
  (not (nil? (some #(= % x) coll))))

(defn vec-repeat
  "Same as clojure.core.repeat but returns a vector instead of a list."
  [n x]
  (vec (repeat n x)))
