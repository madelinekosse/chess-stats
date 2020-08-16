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

(ns magnus.debug)

(def color&type->unicode {:white {:king   "♚"
                                 :queen  "♛"
                                 :rook   "♜"
                                 :bishop "♝"
                                 :knight "♞"
                                 :pawn   "♟︎"}
                         :black {:king   "♔"
                                 :queen  "♕"
                                 :rook   "♖"
                                 :bishop "♗"
                                 :knight "♘"
                                 :pawn   "♙"}})

(defn piece->unicode
  [piece]
  (let [type (:type piece)
        color (:color piece)]
    (or (get-in color&type->unicode [color type]) " ")))

(defn rank->str
  [i rank]
  (let [index (- 8 i)]
    (str index
         " |"
         (apply str (interpose "│" (map piece->unicode rank)))
         "|")))

(defn board->str
  [board]
  (let [board     (reverse (apply mapv vector board))
        frame-top "  ┌─┬─┬─┬─┬─┬─┬─┬─┐\n"
        frame-mid "\n  ├─┼─┼─┼─┼─┼─┼─┼─┤\n"
        frame-bot "\n  └─┴─┴─┴─┴─┴─┴─┴─┘\n"
        letters   "   a b c d e f g h"]
    (str frame-top
         (apply str (interpose frame-mid
                               (map-indexed rank->str board)))
         frame-bot
         letters)))

(defn print-board
  [state]
  (println (board->str (:board state))))
