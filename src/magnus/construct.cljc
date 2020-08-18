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

(ns magnus.construct
  "Functions related to creating and modifying data structures used in Magnus."
  (:require [clojure.test :refer [is]]))

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

(defn- vec-repeat
  "Same as clojure.core.repeat but returns a vector instead of a list."
  [n x]
  (vec (repeat n x)))

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
  {:board            standard-board
   :player-in-turn   :white
   :castling         {:white {:kingside  true
                              :queenside true}
                      :black {:kingside  true
                              :queenside true}}
   :en-passant       nil
   :en-passant-timer 0})

(def new-blank-game
  "Beginning state for a chess game but with no pieces on the board."
  (assoc new-game :board blank-board))

(def all-squares
  "All possible file rank tuples for a chessboard."
  (for [file (range 8) rank (range 8)]
    [file rank]))

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

(defn set-moved
  "Mark piece at square as having been moved."
  {:test (fn []
           (is (-> new-game
                   (set-moved [0 0])
                   (get-piece [0 0])
                   :moved?)))}
  [state [file rank]]
  (update-in state [:board file rank :moved?] (fn [_] true)))

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
