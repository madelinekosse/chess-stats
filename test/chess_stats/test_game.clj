(ns chess-stats.test-game
  (:require  [clojure.test :refer :all]
             [chess-stats.game :as sut]
             [chess-stats.game-state.construct :as state]))


(def test-game [{:white {:move {:piece :pawn, :to {:file :e, :rank :4}}}
                 :black {:move {:piece :pawn, :to {:file :e, :rank :5}}}}
                {:white {:move {:piece :knight, :to {:file :c, :rank :3}}}
                 :black {:move {:piece :knight, :to {:file :c, :rank :6}}}}
                {:white {:move {:piece :bishop, :to {:file :c, :rank :4}}}
                 :black {:move {:piece :knight, :to {:file :f, :rank :6}}}}
                {:white {:move {:piece :knight, :to {:file :f, :rank :3}}}
                 :black {:move {:piece :pawn, :to {:file :d, :rank :5}}}}
                {:white {:move {:piece :pawn, :from {:file :e}, :to {:file :d, :rank :5}}}
                 :black {:move {:piece :knight, :to {:file :d, :rank :5}}}}
                {:white {:move {:piece :knight, :to {:file :d, :rank :5}}}
                 :black {:move {:piece :bishop, :to {:file :g, :rank :4}}}}
                {:white {:move {:castles :kingside}}
                 :black {:move {:piece :bishop, :to {:file :c, :rank :5}}}}
                {:white {:move {:piece :queen, :to {:file :e, :rank :1}}}
                 :black {:move {:castles :kingside}}}
                {:white {:move {:piece :pawn, :to {:file :d, :rank :3}}}
                 :black {:move {:piece :rook, :to {:file :e, :rank :8}}}}
                {:white {:move {:piece :bishop, :to {:file :g, :rank :5}}}
                 :black {:move {:piece :pawn, :to {:file :f, :rank :6}}}}
                {:white {:move {:piece :knight, :to {:file :f, :rank :6}}}
                 :black {:move {:piece :king, :to {:file :h, :rank :8}}}}
                {:white {:move {:piece :knight, :to {:file :e, :rank :8}}}
                 :black {:move {:piece :queen, :to {:file :e, :rank :8}}}}
                {:white {:move {:piece :pawn, :to {:file :c, :rank :3}}}
                 :black {:move {:piece :knight, :to {:file :d, :rank :4}}}}
                {:white {:move {:piece :pawn, :from {:file :c}, :to {:file :d, :rank :4}}}
                 :black {:move {:piece :pawn, :from {:file :e}, :to {:file :d, :rank :4}}}}
                {:white {:move {:piece :queen, :to {:file :d, :rank :2}}}
                 :black {:move {:piece :pawn, :to {:file :h, :rank :6}}}}
                {:white {:move {:piece :rook, :from {:file :f}, :to {:file :e, :rank :1}}}
                 :black {:move {:piece :queen, :to {:file :c, :rank :6}}}}
                {:white {:move {:piece :knight, :to {:file :e, :rank :5}}}
                 :black {:move {:piece :bishop, :to {:file :b, :rank :4}}}}
                {:white {:move {:piece :knight, :to {:file :c, :rank :6}}}
                 :black {:move {:piece :bishop, :to {:file :d, :rank :2}}}}
                {:white {:move {:piece :rook, :to {:file :e, :rank :4}}}
                 :black {:move {:piece :pawn, :from {:file :h}, :to {:file :g, :rank :5}}}}
                {:white {:move {:piece :rook, :to {:file :g, :rank :4}}}
                 :black {:move {:piece :rook, :to {:file :e, :rank :8}}}}
                {:white {:move {:piece :rook, :to {:file :e, :rank :4}}}
                 :black {:move {:piece :rook, :to {:file :f, :rank :8}}}}
                {:white {:move {:piece :pawn, :to {:file :f, :rank :3}}}
                 :black {:move {:piece :pawn, :from {:file :b}, :to {:file :c, :rank :6}}}}
                {:white {:move {:piece :rook, :to {:file :d, :rank :1}}}
                 :black {:move {:piece :bishop, :to {:file :e, :rank :3}}}}
                {:white {:move {:piece :king, :to {:file :h, :rank :1}}}
                 :black {:move {:piece :pawn, :to {:file :g, :rank :4}}}}
                {:white {:move {:piece :rook, :to {:file :g, :rank :4}}}
                 :black nil}])

(deftest test-game-state
  (let [result (sut/add-game-state-to-move-list test-game)]
    (testing "Correct state after castling"
      (let [state-after-white-castles (sut/state-after-move result
                                                            7
                                                            :white)]
        (testing "King is moved"
          (is (= (state/get-piece state-after-white-castles [6 0])
                 {:type :king
                  :color :white
                  :moved? true})))
        (testing "Rook is moved"
          (is (= (state/get-piece state-after-white-castles [5 0])
                 {:type :rook
                  :color :white
                  :moved? true})))))

    (testing "Where the last move is made by white, final game state keyed under whites last move"
      (let [ending-game-state (sut/end-state result)]
        (testing "Final move included in state"
          (is (= (state/get-piece ending-game-state [6 3])
                 {:type :rook
                  :color :white
                  :moved? true})))))

    ))
