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

(ns clochess.core
  (:require [clojure.test :refer [is]]
            [clochess.construct :refer :all]
            [clochess.debug :refer :all]))

(defn file-rank-to-string
  {:test (fn []
           (is (= (file-rank-to-string [0 0])
                  (file-rank-to-string 0 0)
                  "a1"))
           (is (= (file-rank-to-string [7 7])
                  (file-rank-to-string 7 7)
                  "h8"))
           (is (= (file-rank-to-string [3 4])
                  (file-rank-to-string 3 4)
                  "d5")))}
  ([coords]
   (apply file-rank-to-string coords))
  ([file rank]
   (str (char (+ 97 file)) (inc rank))))

(defn string-to-file-rank
  {:test (fn []
           (is (= (string-to-file-rank "a1")
                  [0 0]))
           (is (= (string-to-file-rank "h8")
                  [7 7]))
           (is (= (string-to-file-rank "d5")
                  [3 4])))}
  [s]
  (let [file (int (first s))
        rank (int (second s))]
    [(- file 97)
     (- rank 49)]))

(defn out-of-bounds?
  {:test (fn []
           (is (not-any? out-of-bounds? [[1 3] [6 2] [0 0] [7 7]]))
           (is (every? out-of-bounds? [[-1 4] [1 8] [9 9] [-3 -4]])))}
  ([[file rank]]
   (out-of-bounds? file rank))
  ([file rank]
   (not (and (<= 0 file 7)
             (<= 0 rank 7)))))

(defn free?
  {:test (fn []
           (is (-> (new-game)
                   (free? 4 4)))
           (is (not (-> (new-game)
                        (free? 0 0)))))}
  ([state [file rank]]
   (free? state file rank))
  ([state file rank]
   (= (get-piece state file rank) nil)))

(defn friendly?
  {:test (fn []
           (is (-> (new-game)
                   (friendly? 0 0 :white)))
           (is (-> (new-game)
                   (friendly? 7 7 :black)))
           (is (not (-> (new-game)
                        (friendly? 0 0 :black))))
           (is (not (-> (new-game)
                        (friendly? 7 7 :white)))))}
  ([state [file rank] color]
   (friendly? state file rank color))
  ([state file rank color]
  (let [piece (get-piece state file rank)]
    (= (:color piece) color))))

(def opposite-color
  {:white :black
   :black :white})

(defn enemy?
  {:test (fn []
           (is (-> (new-game)
                   (enemy? 0 0 :black)))
           (is (-> (new-game)
                   (enemy? 7 7 :white)))
           (is (not (-> (new-game)
                        (enemy? 0 0 :white))))
           (is (not (-> (new-game)
                        (enemy? 7 7 :black))))
           (is (not (-> (new-game)
                        (enemy? 4 4 :black)))))}
  [state file rank color]
  (let [piece (get-piece state file rank)]
    (= (:color piece) (color opposite-color))))

(defn capture-possible?
  {:test (fn []
           (is (-> (new-game)
                   (capture-possible? 0 0 :black)))
           (is (not (-> (new-game)
                        (capture-possible? 0 0 :white))))
           (is (not (-> (new-game)
                        (capture-possible? 4 4 :white)))))}
  [state file rank color]
  (and (not (nil? file))
       (not (nil? rank))
       (enemy? state file rank color)))

(defn remove-blocked
  {:test (fn []
           (is (= (-> (new-game)
                      (remove-blocked :white
                                      [[4 4] [4 5] [4 6] [4 7] [4 8]])))
               [[4 4] [4 5] [4 6] [4 7]])
           (is (= (-> (new-game)
                      (remove-blocked :black
                                      [[4 4] [4 5] [4 6] [4 7] [4 8]])))
               [[4 4] [4 5] [4 6]])
           (is (= (-> (new-game)
                      (remove-blocked :white
                                      [[4 4] [4 3] [4 2] [4 1] [4 0]])))
               [[4 4] [4 3] [4 2]])
           (is (= (-> (new-game)
                      (remove-blocked :black
                                      [[4 4] [4 3] [4 2] [4 1] [4 0]])))
               [[4 4] [4 3] [4 2] [4 1]]))}
  [state color coords]
  (let [[free not-free]   (split-with #(free? state %) coords)
        [file rank]       (first not-free)]
    (if (capture-possible? state file rank color)
      (conj free [file rank])
      free)))

(defn valid-moves-king
  {:test (fn []
           (is (-> (new-game)
                   (valid-moves-king 4 4 :white)
                   (count))
               8)
           (is (-> (new-game)
                   (valid-moves-king 0 0 :white)
                   (empty?)))
           (is (every? (set (-> (new-game)
                                (valid-moves-king 4 4 :white)))
                       [[3 5] [4 5] [5 5]
                        [3 4] [5 4]
                        [3 3] [4 3] [5 3]]))
           (is (every? (set (-> (new-game)
                                (valid-moves-king 4 5 :white)))
                       [[3 6] [4 6] [5 6]
                        [3 5] [5 5]
                        [3 4] [4 4] [5 4]]))
           (is (every? (set (-> (new-game)
                                (valid-moves-king 4 5 :black)))
                       [[3 5] [5 5]
                        [3 4] [4 4] [5 4]])))}
  [state file rank color]
  (let [surrounding [[(dec file) (inc rank)]
                     [(dec file) rank]
                     [(dec file) (dec rank)]
                     [file (inc rank)]
                     [file (dec rank)]
                     [(inc file) (inc rank)]
                     [(inc file) rank]
                     [(inc file) (dec rank)]]]
    (remove out-of-bounds? (remove #(friendly? state % color) surrounding))))

(defn valid-moves-rook
  {:test (fn []
           (is (-> (new-game)
                   (valid-moves-rook 0 0 :white)
                   (empty?)))
           (is (every? (set (-> (new-game)
                                (valid-moves-rook 4 4 :white)))
                       [[0 4] [1 4] [2 4] [3 4] [5 4] [6 4]
                        [7 4] [4 3] [4 2] [4 5] [4 6]]))
           (is (every? (set (-> (new-game)
                                (valid-moves-rook 4 4 :black)))
                       [[0 4] [1 4] [2 4] [3 4] [5 4] [6 4]
                        [7 4] [4 1] [4 3] [4 2] [4 5]])))}
  [state file rank color]
  (let [north (map vector (repeat file) (range (inc rank) 8))
        south (map vector (repeat file) (range (dec rank) -1 -1))
        east  (map vector (range (inc file) 8) (repeat rank))
        west  (map vector (range (dec file) -1 -1) (repeat rank))]
    (apply concat
           (map #(remove-blocked state color %)
                [north south east west]))))

(defn valid-moves-bishop
  {:test (fn []
           (is (-> (new-game)
                   (valid-moves-bishop 2 0 :white)
                   (empty?)))
           (is (every? (set (-> (new-game)
                                (valid-moves-bishop 4 4 :white)))
                       [[3 5] [2 6]
                        [5 5] [6 6]
                        [3 3] [2 2]
                        [5 3] [6 2]]))
           (is (every? (set (-> (new-game)
                                (valid-moves-bishop 4 4 :black)))
                       [[3 5]
                        [5 5]
                        [3 3] [2 2] [1 1]
                        [5 3] [6 2] [7 1]])))}
  [state file rank color]
  (let [nw  (map vector (range (dec file) -1 -1) (range (inc rank) 8))
        sw  (map vector (range (dec file) -1 -1) (range (dec file) -1 -1))
        ne  (map vector (range (inc file) 8) (range (inc rank) 8 ))
        se  (map vector (range (inc file) 8) (range (dec file) -1 -1))]
    (apply concat
           (map #(remove-blocked state color %)
                [nw sw ne se]))))

(defn valid-moves-queen
  {:test (fn []
           (is (-> (new-game)
                   (valid-moves-queen 0 0 :white)
                   (empty?)))
           (is (= (-> (new-game)
                      (valid-moves-queen 4 4 :white)
                      (count))
                  (-> (new-game)
                      (valid-moves-queen 4 4 :black)
                      (count))
                  19)))}
  [state file rank color]
  (concat (valid-moves-rook state file rank color)
          (valid-moves-bishop state file rank color)))

(defn valid-moves-knight
  {:test (fn []
           (is (= (-> (new-game)
                      (valid-moves-knight 0 0 :white))
                  '([1 2])))
           (is (every? (set (-> (new-game)
                                (valid-moves-knight 4 4 :white)))
                       [[3 6] [5 6]
                        [6 5] [6 3]
                        [3 2] [5 2]
                        [2 5] [2 3]]))
           (is (every? (set (-> (new-game)
                                (valid-moves-knight 4 4 :black)))
                       [[6 5] [6 3]
                        [3 2] [5 2]
                        [2 5] [2 3]])))}
  [state file rank color]
  (let [jumps [[(+ file 2) (+ rank 1)]
               [(+ file 2) (- rank 1)]
               [(- file 2) (+ rank 1)]
               [(- file 2) (- rank 1)]
               [(+ file 1) (+ rank 2)]
               [(- file 1) (+ rank 2)]
               [(+ file 1) (- rank 2)]
               [(- file 1) (- rank 2)]]]
    (remove out-of-bounds? (remove #(friendly? state % color) jumps))))

(defn valid-moves-pawn
  [state file rank color]
  (let [moved?    (:moved? (get-piece state file rank))
        direction (:color {:white 1
                           :black -1})]
    nil))

(defn valid-moves
  [state file rank]
  (let [piece            (get-piece state file rank)
        color            (:color piece)
        type             (:type piece)
        valid-moves-fns  {:king   valid-moves-king
                          :queen  valid-moves-queen
                          :rook   valid-moves-rook
                          :bishop valid-moves-bishop
                          :knight valid-moves-knight
                          :pawn   valid-moves-pawn
                          nil     (fn [& _] '())}
        valid-moves-fn   (get valid-moves-fns type)]
    (valid-moves-fn state file rank color)))
