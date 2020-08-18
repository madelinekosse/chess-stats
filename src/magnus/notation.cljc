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

(ns magnus.notation
  "Everything related to Chess notation."
  (:require [clojure.test :refer [is]]))

(defn file&rank->str
  "Converts rank and file as integers into an algebraic notation string"
  {:test (fn []
           (is (= (file&rank->str [0 0])
                  "a1"))
           (is (= (file&rank->str [7 7])
                  "h8"))
           (is (= (file&rank->str [3 4])
                  "d5")))}
  [[file rank]]
  (str (char (+ 97 file)) (inc rank)))

(defn str->file&rank
  "Converts a string representing a square in algebraic
   notation into an integer tuple."
  {:test (fn []
           (is (= (str->file&rank "a1")
                  [0 0]))
           (is (= (str->file&rank "h8")
                  [7 7]))
           (is (= (str->file&rank "d5")
                  [3 4])))}
  [s]
  (let [file (int (first s))
        rank (int (second s))]
    [(- file 97)
     (- rank 49)]))
