(ns chess-stats.pgn
  (:require [clojure.test :refer [is]]
            [clj-pgn.core :as clj-pgn]
            [cheshire.core :refer [parse-string]]
            [chess-stats.moves :as moves]
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
       (moves/add-game-state-to-move-list)))


(defn merge-headers
  "Convert list of single-entry maps to a single map, stripping quotes"
  {:test (fn [] (is (= (merge-headers [{:Event "\"Live Chess\""} {:Site "\"Chess.com\""}])
                       {:Event "Live Chess" :Site "Chess.com"})))}
  [headers]
  (->> headers
       (apply merge)
       (reduce-kv (fn [m k v]
                    (assoc m k (parse-string v)))
                  {})))

(defn load-game
  [filename]
  (-> filename
      clj-pgn/load-pgn
      first
      (update :movelist enrich-movelist)
      (update :headers merge-headers)))
