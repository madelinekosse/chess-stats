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
            [clochess.construct :refer [all-squares
                                        get-piece
                                        new-blank-game
                                        new-game
                                        new-piece
                                        set-piece]]
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
  ([squares]
   (apply file&rank->str squares))
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
   (= (get-piece state file rank)
      nil)))

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
  (-> (get-piece state file rank)
      (:color)
      (= color))))

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
   (-> (get-piece state file rank)
       (:color)
       (= (color opposite-color)))))

(defn type?
  "True if piece at the given square is of the given type. Otherwise false."
  {:test (fn []
           (is (-> new-game
                   (type? :king 4 0)))
           (is (not (-> new-game
                        (type? :pawn 0 0)))))}
  ([state type [file rank]]
   (type? state type file rank))
  ([state type file rank]
   (-> (get-piece state file rank)
       (:type)
       (= type))))

(defn remove-blocked
  "Takes a list of rank file tuples representing a line of valid moves for a bishop, rook, or queen,
     and cuts it off at the first square with a blocking piece.
     If that piece is an enemy piece, it is also included in the returning list"
  {:test (fn []
           (is (-> new-game
                      (remove-blocked :white
                                      [[4 4] [4 5] [4 6] [4 7] [4 8]]))
               [[4 4] [4 5] [4 6] [4 7]])
           (is (-> new-game
                      (remove-blocked :black
                                      [[4 4] [4 5] [4 6] [4 7] [4 8]]))
               [[4 4] [4 5] [4 6]])
           (is (-> new-game
                      (remove-blocked :white
                                      [[4 4] [4 3] [4 2] [4 1] [4 0]]))
               [[4 4] [4 3] [4 2]])
           (is (-> new-game
                      (remove-blocked :black
                                      [[4 4] [4 3] [4 2] [4 1] [4 0]]))
               [[4 4] [4 3] [4 2] [4 1]]))}
  [state color squares]
  (let [[free not-free]   (split-with (partial free? state) squares)
        [file rank]       (first not-free)]
    (if (enemy? state color file rank)
      (conj free [file rank])
      free)))

(defn valid-moves-king
  "Returns a list of rank-file tuples representing
     valid moves for a king at file and rank"
  {:test (fn []
           (is (-> new-game
                   (valid-moves-king :white 4 4)
                   (count))
               8)
           (is (-> new-game
                   (valid-moves-king :white 0 0)
                   (empty?)))
           (is (= (set (-> new-game
                           (valid-moves-king :white 4 4)))
                  (set [[3 5] [4 5] [5 5]
                        [3 4] [5 4]
                        [3 3] [4 3] [5 3]])))
           (is (= (set (-> new-game
                           (valid-moves-king :white 4 5)))
                  (set [[3 6] [4 6] [5 6]
                        [3 5] [5 5]
                        [3 4] [4 4] [5 4]])))
           (is (= (set (-> new-game
                           (valid-moves-king :black 4 5)))
                  (set [[3 5] [5 5]
                        [3 4] [4 4] [5 4]]))))}
  [state color file rank]
  (let [surrounding [[(dec file) (inc rank)]
                     [(dec file) rank]
                     [(dec file) (dec rank)]
                     [file       (inc rank)]
                     [file       (dec rank)]
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
                   (valid-moves-rook :white 0 0)
                   (empty?)))
           (is (= (set (-> new-game
                           (valid-moves-rook :white 4 4)))
                  (set [[0 4] [1 4] [2 4] [3 4] [5 4] [6 4]
                        [7 4] [4 3] [4 2] [4 5] [4 6]])))
           (is (= (set (-> new-game
                           (valid-moves-rook :black 4 4)))
                  (set [[0 4] [1 4] [2 4] [3 4] [5 4] [6 4]
                        [7 4] [4 1] [4 3] [4 2] [4 5]]))))}
  [state color file rank]
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
                   (valid-moves-bishop :white 2 0)
                   (empty?)))
           (is (= (set (-> new-game
                           (valid-moves-bishop :white 4 4)))
                  (set [[3 5] [2 6] [5 5] [6 6] [3 3] [2 2] [5 3] [6 2]])))
           (is (= (set (-> new-game
                           (valid-moves-bishop :black 4 4)))
                  (set [[3 5] [5 5] [3 3] [2 2] [1 1] [5 3] [6 2] [7 1]]))))}
  [state color file rank]
  (let [nw  (map vector (range (dec file) -1 -1) (range (inc rank) 8))
        sw  (map vector (range (dec file) -1 -1) (range (dec rank) -1 -1))
        ne  (map vector (range (inc file) 8) (range (inc rank) 8 ))
        se  (map vector (range (inc file) 8) (range (dec rank) -1 -1))]
    (->> [nw sw ne se]
         (map (partial remove-blocked state color))
         (apply concat))))

(defn valid-moves-queen
  "Returns a list of rank-file tuples representing
     valid moves for a queen at file and rank"
  {:test (fn []
           (is (-> new-game
                   (valid-moves-queen :white 0 0)
                   (empty?)))
           (is (= (-> new-game
                      (valid-moves-queen :white 4 4)
                      (count))
                  (-> new-game
                      (valid-moves-queen :black 4 4)
                      (count))
                  19)))}
  [state color file rank]
  (concat (valid-moves-rook state color file rank)
          (valid-moves-bishop state color file rank)))

(defn valid-moves-knight
  "Returns a list of rank-file tuples representing
     valid moves for a knight at file and rank"
  {:test (fn []
           (is (= (-> new-game
                      (valid-moves-knight :white 0 0))
                  '([1 2])))
           (is (= (set (-> new-game
                           (valid-moves-knight :white 4 4)))
                  (set [[3 6] [5 6]
                        [6 5] [6 3]
                        [3 2] [5 2]
                        [2 5] [2 3]])))
           (is (= (set (-> new-game
                           (valid-moves-knight :black 4 4)))
                  (set [[6 5] [6 3]
                        [3 2] [5 2]
                        [2 5] [2 3]]))))}
  [state color file rank]
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
                           (valid-moves-pawn :white 1 1)))
                  (set [[1 2] [1 3]])))
           (is (= (-> new-game
                      (set-piece (assoc (new-piece :pawn :white)
                                        :moved?
                                        true)
                                 2 2)
                      (valid-moves-pawn :white 2 2))
                  '([2 3])))
           (is (= (set (-> new-game
                           (set-piece (new-piece :pawn :black) 2 2)
                           (valid-moves-pawn :white 1 1)))
                  (set [[1 2] [1 3] [2 2]])))
           (is (empty? (-> new-game
                           (set-piece (new-piece :pawn :black) 0 5)
                           (set-piece (new-piece :pawn :black) 1 5)
                           (set-piece (new-piece :pawn :black) 2 5)
                           (valid-moves-pawn :black 1 6)))))}
  [state color file rank]
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
   (let [{:keys [color type]} (get-piece state file rank)
         valid-moves-fns      {:king   valid-moves-king
                               :queen  valid-moves-queen
                               :rook   valid-moves-rook
                               :bishop valid-moves-bishop
                               :knight valid-moves-knight
                               :pawn   valid-moves-pawn
                               nil     (fn [& _] '())}
         valid-moves-fn       (get valid-moves-fns type)]
     (valid-moves-fn state color file rank))))

(defn king-position
  "Get rank-file tuple for king of given color's position.
     Returns nil if no king of given color is found."
  {:test (fn []
           (is (= (-> new-game
                      (king-position :white))
                  [4 0])))}
  [state color]
  (->> all-squares
       (filter (partial friendly? state color))
       (filter (partial type? state :king))
       (first)))

(defn check?
  "True if color is in check, otherwise false."
  {:test (fn []
           (is (-> new-blank-game
                   (set-piece (new-piece :king :white) 1 1)
                   (set-piece (new-piece :pawn :black) 2 2)
                   (check? :white)))
           (is (not (-> new-blank-game
                        (set-piece (new-piece :king :white) 1 1)
                        (set-piece (new-piece :bishop :black) 3 3)
                        (check? :black))))
           (is (not (-> new-blank-game
                        (set-piece (new-piece :king :white) 1 1)
                        (set-piece (new-piece :pawn :black) 3 3)
                        (check? :white))))
           (is (not (-> new-blank-game
                        (set-piece (new-piece :king :white) 1 1)
                        (set-piece (new-piece :pawn :black) 3 3)
                        (set-piece (new-piece :bishop :black) 5 5)
                        (check? :white)))))}
  [state color]
  (->> all-squares
       (filter (partial enemy? state color))
       (map (partial valid-moves state))
       (apply concat)
       (in? (king-position state color))))

(defn valid-move?
  "True if a move from the starting square to the target square is valid.
   Otherwise false."
  {:test (fn []
           (is (-> new-game
                   (valid-move? 1 1 1 2)))
           (is (not (-> new-game
                        (valid-move? 1 1 1 4)))))}
  [state starting-file starting-rank target-file target-rank]
  (in? [target-file target-rank]
       (valid-moves state starting-file starting-rank)))

(defn move
  "Attempt to move piece at starting square to target square.
     Returns state unchanged if move is not valid."
  {:test (fn []
           (is (= (-> new-game
                      (move 1 1 1 2)
                      (get-piece 1 2)
                      (:type))
                  :pawn))
           (is (nil? (-> new-game
                         (move 1 1 1 2)
                         (get-piece 1 1)
                         (:type)))))}
  [state starting-file starting-rank target-file target-rank]
  (if (valid-move? state starting-file starting-rank target-file target-rank)
    (as-> (get-piece state starting-file starting-rank) $
      (set-piece state $ target-file target-rank)
      (set-piece $ nil starting-file starting-rank))
    state))

(defn move->check?
  "True if the given move puts current player in check. Otherwise false."
  {:test (fn []
           (is (-> new-blank-game
                   (set-piece (new-piece :king :white) 0 0)
                   (set-piece (new-piece :bishop :black) 1 2)
                   (move->check? 0 0 0 1)))
           (is (-> new-blank-game
                   (set-piece (new-piece :king :white) 0 0)
                   (set-piece (new-piece :pawn :white) 1 1)
                   (set-piece (new-piece :bishop :black) 3 3)
                   (move->check? 1 1 1 2))))}
  ([state starting-file starting-rank [target-file target-rank]]
   (move->check? state starting-file starting-rank target-file target-rank))
  ([state starting-file starting-rank target-file target-rank]
   (-> (move state starting-file starting-rank target-file target-rank)
       (check? (:player-in-turn state)))))

(defn end-turn
  "End turn and set player-in-turn to opposite player."
  {:test (fn []
           (is (= (-> new-game
                      (end-turn)
                      (:player-in-turn))
                  :black))
           (is (= (-> new-game
                      (end-turn)
                      (end-turn)
                      (:move-number))
                  2)))}
  [state]
  (-> (if (= (:player-in-turn state)
             :black)
        (update state :move-number inc)
        state)
      (update :player-in-turn opposite-color)))
