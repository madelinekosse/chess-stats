(ns chess-stats.util.test-utils
  (:require  #?(:clj [clojure.test :as t]
                :cljs [cljs.test :as t :include-macros true])
             [chess-stats.game-state.construct :as state]
             [chess-stats.pgn :refer [load-game]]))

(def sample-game (load-game "data/mkosse_vs_Enribari74_2020.11.21.pgn"))

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
