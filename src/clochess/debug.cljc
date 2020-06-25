(ns clochess.debug)

;; Simple Graphics useful for debugging
(def color-type-unicode { :white { :king   "♚"
                                   :queen  "♛"
                                   :rook   "♜"
                                   :bishop "♝"
                                   :knight "♞"
                                   :pawn   "♟︎" }
                          :black { :king   "♔"
                                   :queen  "♕"
                                   :rook   "♖"
                                   :bishop "♗"
                                   :knight "♘"
                                   :pawn   "♙" }})
(defn piece-to-unicode [piece]
    (let [type (:type piece)
          color (:color piece)]
        (or (get-in color-type-unicode [color type]) " ")))

(defn rank-to-string [i rank]
    (let [index (- 8 i)]
        (str index
             " |"
             (apply str (interpose "│" (map piece-to-unicode rank)))
             "|")))

(defn board-to-string [board]
    (let [frame-top "  ┌─┬─┬─┬─┬─┬─┬─┬─┐\n"
          frame-mid "\n  ├─┼─┼─┼─┼─┼─┼─┼─┤\n"
          frame-bot "\n  └─┴─┴─┴─┴─┴─┴─┴─┘\n"
          letters   "   a b c d e f g h"]
        (str frame-top
            (apply str (interpose frame-mid
                                  (map-indexed rank-to-string board)))
            frame-bot
            letters)))

(defn print-board [state]
    (println (board-to-string (:board state))))
