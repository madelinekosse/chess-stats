(ns clochess.core-api
    (:require [clochess.construct :refer [new-board
                                          get-piece]]
              [clochess.core :refer [valid-moves-king
                                     valid-moves-queen
                                     valid-moves-rook
                                     valid-moves-bishop
                                     valid-moves-knight
                                     valid-moves-pawn]]))

(defn new-game []
    { :board          (new-board)
      :player-in-turn :white
      :turn           1 })

(defn valid-moves [state file rank]
    (let [type       (:type (get-piece state file rank))
          type-moves { :king   valid-moves-king
                       :queen  valid-moves-queen
                       :rook   valid-moves-rook
                       :bishop valid-moves-bishop
                       :knight valid-moves-knight
                       :pawn   valid-moves-pawn }
          moves-fn   (get type-moves type)]
        (if (nil? moves-fn)
            []
            (moves-fn state file rank))))
