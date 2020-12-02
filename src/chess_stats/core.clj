(ns chess-stats.core
  (:require [chess-stats
             [pgn :as pgn]
             [moves :as moves]]))

(defn new [filename] (pgn/load-game filename))

(defn result
  ":BLACKWINS, :WHITEWINS, or :DRAW"
  [game]
  (:result game))

(defn end-position
  "The final position of the game"
  [game]
  (-> game
      :movelist
      moves/end-state))

(defn position-after-move
  "Position resulting from the given move number"
  [game number color]
  (-> game
      :movelist
      (moves/state-after-move number color)))

(defn headers
  [game]
  (:headers game))
