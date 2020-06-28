(ns clochess.construct)

(defn get-piece
  [state file rank]
  (get-in state [:board file rank]))

(defn set-piece
  [state file rank piece]
  (assoc-in state [:board file rank] piece))

(defn new-piece
  [type color]
  {:type   type
   :color  color
   :moved? false})

(defn new-back-rank
  [color]
  [(new-piece :rook color)
   (new-piece :knight color)
   (new-piece :bishop color)
   (new-piece :queen color)
   (new-piece :king color)
   (new-piece :bishop color)
   (new-piece :knight color)
   (new-piece :rook color)])

(defn vec-repeat
  [n x]
  (vec (repeat n x)))

(defn new-pawn-rank
  [color]
  (vec-repeat 8 (new-piece :pawn color)))

(defn new-blank-rank
  []
  (vec-repeat 8 nil))

(defn new-blank-board
  []
  (vec-repeat 8 (vec-repeat 8 nil)))

(defn new-board
  []
  (apply mapv vector [(new-back-rank :black)
                      (new-pawn-rank :black)
                      (new-blank-rank)
                      (new-blank-rank)
                      (new-blank-rank)
                      (new-blank-rank)
                      (new-pawn-rank :white)
                      (new-back-rank :white)]))

(defn new-game
  []
  {:board          (new-board)
   :player-in-turn :white
   :move-number    1})
