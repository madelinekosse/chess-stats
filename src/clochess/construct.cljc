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

(ns clochess.construct
  (:require [clojure.math.combinatorics :refer [permuted-combinations]]
            [clojure.test :refer [is]]
            [clochess.util :refer [vec-repeat]]))

(defn new-piece
  "Create a piece of given type and color."
  [type color]
  {:type   type
   :color  color
   :moved? false})

(defn new-back-rank
  "Creates a back rank of given color. Pieces are placed like at the start
   of a standard chess game."
  [color]
  [(new-piece :rook color)
   (new-piece :knight color)
   (new-piece :bishop color)
   (new-piece :queen color)
   (new-piece :king color)
   (new-piece :bishop color)
   (new-piece :knight color)
   (new-piece :rook color)])

(defn pawn-rank
  "Creates a rank filled with pawns of the given color."
  {:test (fn []
           (is (every? #{(new-piece :pawn :white)} (pawn-rank :white))))}
  [color]
  (vec-repeat 8 (new-piece :pawn color)))

(def blank-rank
  "A rank with no pieces on it."
  (vec-repeat 8 nil))

(def blank-board
  "A board with no pieces on it."
  (vec-repeat 8 (vec-repeat 8 nil)))


(def standard-board
  "A board with the initial position of standard chess."
  (apply mapv vector [(new-back-rank :white)
                      (pawn-rank :white)
                      blank-rank
                      blank-rank
                      blank-rank
                      blank-rank
                      (pawn-rank :black)
                      (new-back-rank :black)]))

(def new-game
  "Beginning state for a standard chess game."
  {:board          standard-board
   :player-in-turn :white
   :move-number    1
   :halfmove-clock 0
   :en-passant     nil})

(def new-blank-game
  "Beginning state for a chess game but with no pieces on the board."
  {:board          blank-board
   :player-in-turn :white
   :move-number    1
   :halfmove-clock 0
   :en-passant     nil})

(def all-squares
  "All possible file rank tuples for a chessboard."
  (permuted-combinations (concat (range 8)
                                 (range 8))
                         2))

(defn get-piece
  "Get piece at square."
  {:test (fn []
           (is (= (-> new-game
                      (get-piece [0 0]))
                  (new-piece :rook :white)))
           (is (= (-> new-game
                      (get-piece [0 7]))
                  (new-piece :rook :black)))
           (is (nil? (-> new-game
                         (get-piece [4 4])))))}
  [state [file rank]]
  (get-in state [:board file rank]))

(defn set-piece
  "Place piece at square."
  {:test (fn []
           (is (= (-> new-game
                     (set-piece (new-piece :knight :white) [0 0])
                     (get-piece [0 0]))
                  (new-piece :knight :white))))}
  [state piece [file rank]]
  (assoc-in state [:board file rank] piece))

(defn clear-square
  "Clear square of any piece currently occupying it."
  {:test (fn []
           (is (nil? (-> new-game
                         (clear-square [0 0])
                         (get-piece [0 0])))))}
  [state square]
  (set-piece state nil square))

(defn get-player-in-turn
  "Returns the player who's turn it currently is."
  {:test (fn []
           (is (= (get-player-in-turn new-game)
                  :white)))}
  [state]
  (:player-in-turn state))
