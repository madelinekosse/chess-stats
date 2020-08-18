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

(ns magnus.api
  "Public API for Magnus.
   Should be sufficient for most general chess programming"
  (:require [potemkin :refer [import-vars]]
            magnus.core
            magnus.construct
            magnus.notation))

(import-vars
 [magnus.core
  
  castle-available?
  valid-moves
  check?
  move
  move->end-turn]
 
 [magnus.construct
  
  new-piece
  blank-board
  standard-board
  new-game
  new-blank-game
  get-piece
  set-piece
  clear-square
  get-player-in-turn]
 
 [magnus.notation

  file&rank->str
  str->file&rank])
