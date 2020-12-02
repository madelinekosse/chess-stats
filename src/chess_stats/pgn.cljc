(ns chess-stats.pgn
  (:require [clojure.test :refer [is]]
            [clj-pgn.core :as clj-pgn]
            [chess-stats.game :as game]
            [chess-stats.util.notation :as notation]))


(defn- parse-one-move [move]
  (case move
    [:KINGCASTLE] {:castles :kingside}
    [:QUEENCASTLE] {:castles :queenside}
    (notation/string->move move)))

(defn- parse-moves
  "Go through all the moves and replace string with move data map"
  {:test (fn[]
           (is (= (parse-moves {:white {:move "d4"}
                                :black {:move "d5"}})
                  {:white {:move {:piece :pawn
                                  :to {:file :d :rank :4}}}
                   :black {:move {:piece :pawn
                                  :to {:file :d :rank :5}}}}))
           (is (= (parse-moves {:white {:move [:KINGCASTLE]}
                                :black {:move [:QUEENCASTLE]}})
                  {:white {:move {:castles :kingside}}
                   :black {:move {:castles :queenside}}}))
           (is (= (parse-moves {:white {:move "d4"}
                                :black nil})
                  {:white {:move {:piece :pawn
                                  :to {:file :d :rank :4}}}
                   :black nil})))}
  [{:keys [black white] :as movepair}]
  (cond-> movepair
    white (update-in [:white :move] parse-one-move)
    black (update-in [:black :move] parse-one-move)))

(defn- enrich-movelist [movelist]
  (->> movelist
       (map parse-moves)
       (game/add-game-state-to-move-list)))

(defn load-game
  [filename]
  (-> filename
      clj-pgn/load-pgn
      first
      (update :movelist enrich-movelist)))
