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
            [clochess.debug :refer :all]
            [clochess.pgn :refer :all]
            [clochess.util :refer [in?]]))

(defn file&rank->str
  "Converts rank and file as integers into an algebraic notation string"
  {:test (fn []
           (is (= (file&rank->str [0 0])
                  (file&rank->str 0 0)
                  "a1"))
           (is (= (file&rank->str [7 7])
                  (file&rank->str 7 7)
                  "h8"))
           (is (= (file&rank->str [3 4])
                  (file&rank->str 3 4)
                  "d5")))}
  ([coords]
   (apply file&rank->str coords))
  ([file rank]
   (str (char (+ 97 file)) (inc rank))))

(defn str->file&rank
  "Converts a string representing a square in algebraic notation into an integer tuple."
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

(defn out-of-bounds?
  "True if the given file rank coordinates are outside the chessboard. Otherwise false."
  {:test (fn []
           (is (not-any? out-of-bounds? [[1 3] [6 2] [0 0] [7 7]]))
           (is (every? out-of-bounds? [[-1 4] [1 8] [9 9] [-3 -4]])))}
  ([[file rank]]
   (out-of-bounds? file rank))
  ([file rank]
   (not (and (<= 0 file 7)
             (<= 0 rank 7)))))

(defn free?
  "True if the given square is not occupied. Otherwise false."
  {:test (fn []
           (is (and (-> new-game
                        (free? 4 4))))
           (is (not (-> new-game
                        (free? 0 0)))))}
  ([state [file rank]]
   (free? state file rank))
  ([state file rank]
   (= (get-piece state file rank) nil)))

(defn friendly?
  "True if piece at the given square is of the given color. Otherwise false."
  {:test (fn []
           (is (-> new-game
                   (friendly? :white 0 0)))
           (is (-> new-game
                   (friendly? :black 7 7)))
           (is (not (-> new-game
                        (friendly? :black 0 0))))
           (is (not (-> new-game
                        (friendly? :white 7 7)))))}
  ([state color [file rank]]
   (friendly? state color file rank))
  ([state color file rank]
  (let [piece (get-piece state file rank)]
    (= (:color piece) color))))

(def opposite-color
  {:white :black
   :black :white})

(defn enemy?
  "True if piece at the given square is of the color opposite to the one given. Otherwise false."
  {:test (fn []
           (is (-> new-game
                   (enemy? :black 0 0)))
           (is (-> new-game
                   (enemy? :white 7 7)))
           (is (not (-> new-game
                        (enemy? :white 0 0))))
           (is (not (-> new-game
                        (enemy? :black 7 7))))
           (is (not (-> new-game
                        (enemy? :black 4 4)))))}
  ([state color [file rank]]
   (enemy? state color file rank))
  ([state color file rank]
   (let [piece (get-piece state file rank)]
     (= (:color piece)
        (color opposite-color)))))

(defn is-type?
  "True if piece at the given square is of the given type. Otherwise false."
  {:test (fn []
           (is (-> new-game
                   (is-type? :king 4 0)))
           (is (not (-> new-game
                        (is-type? :pawn 0 0)))))}
  ([state type [file rank]]
   (is-type? state type file rank))
  ([state type file rank]
   (let [piece (get-piece state file rank)]
     (= (:type piece)
        type))))

(defn remove-blocked
  "Takes a list of rank file tuples representing a line of valid moves for a bishop, rook, or queen,
     and cuts it off at the first square with a blocking piece.
     If that piece is an enemy piece, it is also included in the returning list"
  {:test (fn []
           (is (= (-> new-game
                      (remove-blocked :white
                                      [[4 4] [4 5] [4 6] [4 7] [4 8]])))
               [[4 4] [4 5] [4 6] [4 7]])
           (is (= (-> new-game
                      (remove-blocked :black
                                      [[4 4] [4 5] [4 6] [4 7] [4 8]])))
               [[4 4] [4 5] [4 6]])
           (is (= (-> new-game
                      (remove-blocked :white
                                      [[4 4] [4 3] [4 2] [4 1] [4 0]])))
               [[4 4] [4 3] [4 2]])
           (is (= (-> new-game
                      (remove-blocked :black
                                      [[4 4] [4 3] [4 2] [4 1] [4 0]])))
               [[4 4] [4 3] [4 2] [4 1]]))}
  [state color coords]
  (let [[free not-free]   (split-with (partial free? state) coords)
        [file rank]       (first not-free)
        capture-possible? (and (not (nil? file))
                               (not (nil? rank))
                               (enemy? state color file rank))]
    (if capture-possible?
      (conj free [file rank])
      free)))

(defn valid-moves-king
  "Returns a list of rank-file tuples representing
     valid moves for a king at file and rank"
  {:test (fn []
           (is (-> new-game
                   (valid-moves-king 4 4 :white)
                   (count))
               8)
           (is (-> new-game
                   (valid-moves-king 0 0 :white)
                   (empty?)))
           (is (= (set (-> new-game
                           (valid-moves-king 4 4 :white)))
                  (set [[3 5] [4 5] [5 5]
                        [3 4] [5 4]
                        [3 3] [4 3] [5 3]])))
           (is (= (set (-> new-game
                           (valid-moves-king 4 5 :white)))
                  (set [[3 6] [4 6] [5 6]
                        [3 5] [5 5]
                        [3 4] [4 4] [5 4]])))
           (is (= (set (-> new-game
                           (valid-moves-king 4 5 :black)))
                  (set [[3 5] [5 5]
                        [3 4] [4 4] [5 4]]))))}
  [state file rank color]
  (let [surrounding [[(dec file) (inc rank)]
                     [(dec file) rank]
                     [(dec file) (dec rank)]
                     [file (inc rank)]
                     [file (dec rank)]
                     [(inc file) (inc rank)]
                     [(inc file) rank]
                     [(inc file) (dec rank)]]]
    (->> surrounding
         (remove (partial friendly? state color))
         (remove out-of-bounds?))))

(defn valid-moves-rook
  "Returns a list of rank-file tuples representing
     valid moves for a rook at file and rank"
  {:test (fn []
           (is (-> new-game
                   (valid-moves-rook 0 0 :white)
                   (empty?)))
           (is (= (set (-> new-game
                           (valid-moves-rook 4 4 :white)))
                  (set [[0 4] [1 4] [2 4] [3 4] [5 4] [6 4]
                        [7 4] [4 3] [4 2] [4 5] [4 6]])))
           (is (= (set (-> new-game
                           (valid-moves-rook 4 4 :black)))
                  (set [[0 4] [1 4] [2 4] [3 4] [5 4] [6 4]
                        [7 4] [4 1] [4 3] [4 2] [4 5]]))))}
  [state file rank color]
  (let [north (map vector (repeat file) (range (inc rank) 8))
        south (map vector (repeat file) (range (dec rank) -1 -1))
        east  (map vector (range (inc file) 8) (repeat rank))
        west  (map vector (range (dec file) -1 -1) (repeat rank))]
    (->> [north south east west]
         (map (partial remove-blocked state color))
         (apply concat))))

(defn valid-moves-bishop
  "Returns a list of rank-file tuples representing
     valid moves for a bishop at file and rank"
  {:test (fn []
           (is (-> new-game
                   (valid-moves-bishop 2 0 :white)
                   (empty?)))
           (is (= (set (-> new-game
                           (valid-moves-bishop 4 4 :white)))
                  (set [[3 5] [2 6]
                        [5 5] [6 6]
                        [3 3] [2 2]
                        [5 3] [6 2]])))
           (is (= (set (-> new-game
                           (valid-moves-bishop 4 4 :black)))
                  (set [[3 5] [5 5] [3 3] [2 2]
                        [1 1] [5 3] [6 2] [7 1]]))))}
  [state file rank color]
  (let [nw  (map vector (range (dec file) -1 -1) (range (inc rank) 8))
        sw  (map vector (range (dec file) -1 -1) (range (dec file) -1 -1))
        ne  (map vector (range (inc file) 8) (range (inc rank) 8 ))
        se  (map vector (range (inc file) 8) (range (dec file) -1 -1))]
    (->> [nw sw ne se]
         (map (partial remove-blocked state color))
         (apply concat))))

(defn valid-moves-queen
  "Returns a list of rank-file tuples representing
     valid moves for a queen at file and rank"
  {:test (fn []
           (is (-> new-game
                   (valid-moves-queen 0 0 :white)
                   (empty?)))
           (is (= (-> new-game
                      (valid-moves-queen 4 4 :white)
                      (count))
                  (-> new-game
                      (valid-moves-queen 4 4 :black)
                      (count))
                  19)))}
  [state file rank color]
  (concat (valid-moves-rook state file rank color)
          (valid-moves-bishop state file rank color)))

(defn valid-moves-knight
  "Returns a list of rank-file tuples representing
     valid moves for a knight at file and rank"
  {:test (fn []
           (is (= (-> new-game
                      (valid-moves-knight 0 0 :white))
                  '([1 2])))
           (is (= (set (-> new-game
                           (valid-moves-knight 4 4 :white)))
                  (set [[3 6] [5 6]
                        [6 5] [6 3]
                        [3 2] [5 2]
                        [2 5] [2 3]])))
           (is (= (set (-> new-game
                           (valid-moves-knight 4 4 :black)))
                  (set [[6 5] [6 3]
                        [3 2] [5 2]
                        [2 5] [2 3]]))))}
  [state file rank color]
  (let [jumps [[(+ file 2) (+ rank 1)]
               [(+ file 2) (- rank 1)]
               [(- file 2) (+ rank 1)]
               [(- file 2) (- rank 1)]
               [(+ file 1) (+ rank 2)]
               [(- file 1) (+ rank 2)]
               [(+ file 1) (- rank 2)]
               [(- file 1) (- rank 2)]]]
    (->> jumps
         (remove (partial friendly? state color))
         (remove out-of-bounds?))))

(defn valid-moves-pawn
  "Returns a list of rank-file tuples representing
     valid moves for a pawn at file and rank"
  {:test (fn []
           (is (= (set (-> new-game
                           (valid-moves-pawn 1 1 :white)))
                  (set [[1 2] [1 3]])))
           (is (= (-> new-game
                      (set-piece 2 2 (assoc (new-piece :pawn :white)
                                            :moved?
                                            true))
                      (valid-moves-pawn 2 2 :white))
                  '([2 3])))
           (is (= (set (-> new-game
                           (set-piece 2 2 (new-piece :pawn :black))
                           (valid-moves-pawn 1 1 :white)))
                  (set [[1 2] [1 3] [2 2]])))
           (is (empty? (-> new-game
                           (set-piece 0 5 (new-piece :pawn :black))
                           (set-piece 1 5 (new-piece :pawn :black))
                           (set-piece 2 5 (new-piece :pawn :black))
                           (valid-moves-pawn 1 6 :black)))))}
  [state file rank color]
  (let [moved?    (:moved? (get-piece state file rank))
        direction (color {:white 1
                          :black -1})
        one-step  [[file (+ rank direction)]]
        two-steps (if (not moved?)
                    [[file (+ rank (* direction 2))]]
                    [])
        forward   (concat one-step two-steps)
        diagonals [[(dec file) (+ rank direction)]
                   [(inc file) (+ rank direction)]]]
    (concat (take-while (partial free? state)
                        forward)
            (filter (partial enemy? state color)
                    diagonals))))

(defn valid-moves
  "Returns a list of rank-file tuples representing
     valid moves for the piece at file and rank"
  {:test (fn []
           (is (empty? (-> new-game
                           (valid-moves 3 3)))))}
  ([state [file rank]]
   (valid-moves state file rank))
  ([state file rank]
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
     (valid-moves-fn state file rank color))))

(defn get-king-coords
  "Get rank-file tuple for king of given color's position.
     Returns nil if no king of given color is found."
  {:test (fn []
           (is (= (-> new-game
                      (get-king-coords :white))
                  [4 0])))}
  [state color]
  (->> all-coords
       (filter (partial friendly? state color))
       (filter (partial is-type? state :king))
       (first)))

(defn in-check?
  "True if color is in check, otherwise false."
  {:test (fn []
           (is (-> new-blank-game
                   (set-piece 1 1 (new-piece :king :white))
                   (set-piece 2 2 (new-piece :pawn :black))
                   (in-check? :white)))
           (is (not (-> new-blank-game
                        (set-piece 1 1 (new-piece :king :white))
                        (set-piece 3 3 (new-piece :bishop :black))
                        (in-check? :black))))
           (is (not (-> new-blank-game
                        (set-piece 1 1 (new-piece :king :white))
                        (set-piece 3 3 (new-piece :pawn :black))
                        (in-check? :white))))
           (is (not (-> new-blank-game
                        (set-piece 1 1 (new-piece :king :white))
                        (set-piece 3 3 (new-piece :pawn :black))
                        (set-piece 5 5 (new-piece :bishop :black))
                        (in-check? :white)))))}
  [state color]
  (let [king-coords  (get-king-coords state color)
        enemy-moves  (->> all-coords
                          (filter (partial enemy? state color))
                          (map (partial valid-moves state))
                          (apply concat))]
    (in? king-coords enemy-moves)))
