(ns clochess.core-api)

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
