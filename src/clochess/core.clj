(ns clochess.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;;;; Chess
;;;; by Anders Eriksson

;;; PIECES

(defn set-moved [state file rank]
    { :test (fn [] (assert (= 1 1)))}
    (assoc-in state [:board rank file :moved?] true))

;; King
(defn move-king [state file rank]
    '('((- file 1) (- rank 1))
      '(file       (- rank 1))
      '((+ file 1) (- rank 1))
      '((- file 1) rank)
      '((+ file 1) rank)
      '((- file 1) (+ rank 1))
      '(file       (+ rank 1))
      '((+ file 1) (+ rank 1))))
(defn new-king [color]
    { :type       :king
      :color      color
      :moved?     false })

;; Queen
(defn move-queen [state file rank]
    (println "Queen movement not yet implemented"))
(defn new-queen [color]
    { :type      :queen
      :color     color })

;; Rook
(defn move-rook [state file rank]
    (println "Rook movement not yet implemented"))
(defn new-rook [color]
    { :type       :rook
      :color      color
      :moved?     false })

;; Bishop
(defn move-bishop [state file rank]
    (println "Bishop movement not yet implemented"))
(defn new-bishop [color]
    { :type      :bishop
      :color     color })

;; Knight
(defn move-knight [state file rank]
    (println "Knight movement not yet implemented"))
(defn new-knight [color]
    { :type      :knight
      :color     color })

;; Pawn
(defn move-pawn [state file rank]
    (println "Pawn movement not yet implemented"))
(defn new-pawn [color]
    { :type       :pawn
      :color      color
      :moved?     false })

;;; BOARD
;; Utilities for building starting board
(defn new-back-rank [color]
    [(new-rook color)
     (new-knight color)
     (new-bishop color)
     (new-queen color)
     (new-king color)
     (new-bishop color)
     (new-knight color)
     (new-rook color)])

(defn new-pawn-rank [color]
    (vec (repeat 8 (new-pawn color))))

(defn new-blank-rank []
    (vec (repeat 8 nil)))

(defn new-board []
    [(new-back-rank :black)
     (new-pawn-rank :black)
     (new-blank-rank)
     (new-blank-rank)
     (new-blank-rank)
     (new-blank-rank)
     (new-pawn-rank :white)
     (new-back-rank :white)])

;; Getting and setting pieces on the board
(defn get-piece [state file rank]
    (get-in state [:board rank file]))

(defn set-piece [state file rank piece]
    (assoc-in state [:board rank file] piece))

;; Checks
(defn out-of-bounds? [file rank]
    (not (and (<= 0 file 7)
              (<= 0 rank 7))))

;; General Movement
(defn move-piece [state file rank]
    (let [type      (:type (get-piece state file rank))
          type-move { :king   move-king
                      :queen  move-queen
                      :rook   move-rook
                      :bishop move-bishop
                      :knight move-knight
                      :pawn   move-pawn }
          move      (get type-move type)]
        (if (nil? move)
            []
            (move state file rank))))

;; Simple Graphics
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

;;; GAME
(defn new-game-state []
    { :board          (new-board)
      :player-in-turn :white
      :turn           1 })

;;; DEBUG

;(print-board (new-game-state))
;(move-piece (new-game-state) 1 1)
;(println (new-game-state))
;(println (get-piece (new-game-state) 0 6))
