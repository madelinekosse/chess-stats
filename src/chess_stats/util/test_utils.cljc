(ns chess-stats.util.test-utils
  (:require [chess-stats.game-state.construct :as state]))


(def promotion-endgame (-> state/new-blank-game
                           (state/set-piece {:type :king
                                             :color :white
                                             :moved? true}
                                            [4 0])
                           (state/set-piece {:type :king
                                             :color :black
                                             :moved? true}
                                            [2 2])
                           (state/set-piece {:type :pawn
                                             :color :white
                                             :moved? true}
                                            [4 6])))
