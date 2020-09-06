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
  (:require [clojure.test :refer [is]]
            [clojure.set :refer [union]]
            [magnus.util :refer [vec-repeat]]))

(def white-pieces
  "Set of all keywords representing white pieces."
  #{:P :N :B :R :Q :K})

(def black-pieces
  "Set of all keywords representing black pieces."
  #{:p :n :b :r :q :k})

(def pieces
  "Set of all keywords representing all pieces."
  (union white-pieces black-pieces))

(defn new-piece
  "Returns a piece of given type and color."
  [color type]
  {:type   type
   :color  color})

(defn- new-back-rank
  "Returns a back rank of given color. Pieces are placed like at the start
   of a standard chess game."
  [color]
  [(new-piece color :rook)
   (new-piece color :knight)
   (new-piece color :bishop)
   (new-piece color :queen)
   (new-piece color :king)
   (new-piece color :bishop)
   (new-piece color :knight)
   (new-piece color :rook)])

(defn- pawn-rank
  "Returns a rank filled with pawns of the given color."
  {:test (fn []
           (is (every? #{(new-piece :white :pawn)} (pawn-rank :white))))}
  [color]
  (vec-repeat 8 (new-piece color :pawn)))

(def ^:private blank-rank
  "A rank with no pieces on it."
  (vec-repeat 8 nil))

(def ^:private blank-board
  "A board with no pieces on it."
  (vec-repeat 8 (vec-repeat 8 nil)))


(def ^:private standard-board
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
   :castling         #{:K :Q :k :q}
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
           (is (= (get-piece new-game [0 0])
                  (new-piece :white :rook)))
           (is (= (get-piece new-game [0 7])
                  (new-piece :black :rook)))
           (is (nil? (get-piece new-game [4 4]))))}
  [state [file rank]]
  (get-in state [:board file rank]))

(defn castle-available?
  "True if neither king, nor castle on given side, of 
   color has moved. Otherwise false."
  [state side]
  (contains? (:castling state) side))

(defn remove-castling
  "Removes castling availability on the specified side/sides"
  {:test (fn []
           (is (= (:castling (remove-castling new-game :K))
                  #{:Q :k :q}))
           (is (= (:castling (remove-castling new-game :K :q))
                  #{:Q :k})))}
  [state & sides]
  (update state :castling #(apply disj % sides)))

(defn set-piece
  "Place piece at square."
  {:test (fn []
           (is (= (-> new-game
                      (set-piece (new-piece :white :knight) [0 0])
                      (get-piece [0 0]))
                  (new-piece :white :knight))))}
  [state piece [file rank]]
  (assoc-in state [:board file rank] piece))

(defn set-result
  "Sets result of a game."
  [state result]
  (assoc state :result result))

(defn clear-square
  "Clear square of any piece currently occupying it."
  {:test (fn []
           (is (nil? (-> new-game
                         (clear-square [0 0])
                         (get-piece [0 0])))))}
  [state square]
  (set-piece state nil square))

(defn set-en-passant
  "Set en-passant square."
  [state square]
  (-> (assoc state :en-passant square)
      (assoc :en-passant-timer 2)))

(defn get-en-passant
  "Get en-passant square if there is one, else nil."
  [state]
  (get state :en-passant))

(defn get-player-in-turn
  "Returns the player who's turn it currently is."
  [state]
  (:player-in-turn state))
