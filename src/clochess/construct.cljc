(ns clochess.construct
  (:require [clojure.test :refer [is]]))

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
  {:test (fn []
           (is (every? #(= % (new-piece :pawn :white)) (new-pawn-rank :white))))}
  [color]
  (vec-repeat 8 (new-piece :pawn color)))

(defn new-blank-rank
  {:test (fn []
           (is (every? nil? (new-blank-rank))))}
  []
  (vec-repeat 8 nil))

(defn new-blank-board
  {:test (fn []
           (is (->> (new-blank-board)
                    (flatten)
                    (every? nil?)))
           (is (= (-> (new-blank-board)
                      (flatten)
                      (count))
                  64)))}
  []
  (vec-repeat 8 (vec-repeat 8 nil)))


(defn new-board
  []
  (apply mapv vector [(new-back-rank :white)
                      (new-pawn-rank :white)
                      (new-blank-rank)
                      (new-blank-rank)
                      (new-blank-rank)
                      (new-blank-rank)
                      (new-pawn-rank :black)
                      (new-back-rank :black)]))

(defn new-game
  []
  {:board          (new-board)
   :player-in-turn :white
   :move-number    1})

(defn get-piece
  {:test (fn []
           (is (= (-> (new-game)
                      (get-piece 0 0))
                  (new-piece :rook :white)))
           (is (= (-> (new-game)
                      (get-piece 0 7))
                  (new-piece :rook :black)))
           (is (nil? (-> (new-game)
                         (get-piece 4 4)))))}
  [state file rank]
  (get-in state [:board file rank]))

(defn set-piece
  {:test (fn []
           (is (-> (new-game)
                   (set-piece 0 0 true)
                   (get-piece 0 0))))}
  [state file rank piece]
  (assoc-in state [:board file rank] piece))
