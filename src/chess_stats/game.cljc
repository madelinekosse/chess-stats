(ns chess-stats.game
  (:require [clojure.test :refer [is]]
            [chess-stats.util.test-utils :as tests]
            [chess-stats.game-state.construct :as state]
            [chess-stats.game-state.play :as rules]))

(defn- file->index
  "For a file keyword, return the board index"
  {:test (fn []
           (is (= (file->index :a) 0)))}
  [file]
  (-> file
      name
      first
      int
      (- 97)))

(defn- rank->index
  "For a rank keyword, return the board index"
  {:test (fn []
           (is (= (rank->index :1) 0)))}
  [rank]
  (-> rank
      name
      first
      int
      (- 49)))

(defn- maybe-update [m k f]
  (if (contains? m k)
    (update m k f)
    m))

;;TODO: this is what's breaking when it's nil
(defn- file&rank->indices
  "For a map containing file and/or rank keywords, convert them to board indixes"
  {:test (fn []
           (is (= (file&rank->indices {:file :a})
                  {:file 0}))
           (is (= (file&rank->indices {:rank :8})
                  {:rank 7})))}
  [file&rank]
  (-> file&rank
      (maybe-update :file file->index)
      (maybe-update :rank rank->index)))


(defn- coordinates-to
  "Returns numeric file and rank of the destination square"
  {:test (fn[]
           (is (= (coordinates-to {:to {:file :h
                                        :rank :8}})
                  [7 7]))
           (is (= (coordinates-to {:to {:file :a
                                        :rank :1}})
                  [0 0]))
           (is (= (coordinates-to {:to {:file :d
                                        :rank :5}})
                  [3 4])))}
  [move]
  (->> move
       :to
       file&rank->indices
       ((juxt :file :rank))))

(defn- matching-piece?
  "Check if the given square contains the specifies piece"
  {:test (fn []
           (is (matching-piece? :white :pawn {:piece {:color :white
                                                      :type :pawn}}))
           (is (not (matching-piece? :black :pawn {:piece {:color :white
                                                           :type :pawn}}))))}
  [color piece-type {:keys [piece]}]
  (and (= piece-type (:type piece))
       (= color (:color piece))))

(defn- disambiguate
  "Where multiple pieces could complete the move, select the one specified by the file and/or rank"
  {:test (fn []
           (is (= (disambiguate {:file :d}
                                [[1 0] [3 0]])
                  [3 0])))}
  [file&rank potential-squares]
  (if (= 1 (count potential-squares))
    (first potential-squares)
    (let [filters (file&rank->indices file&rank)
          file-to-match (get filters :file)
          rank-to-match (get filters :rank)]
      (->> potential-squares
           (filter #(if file-to-match
                      (= file-to-match (first %))
                      true))
           (filter #(if rank-to-match
                      (= rank-to-match (last %))
                      true))
           first))))

(defn- coordinates-from
  "For the given move and board, find the square the piece moved from"
  {:test (fn[]
           (is (= (coordinates-from
                   state/new-game
                   {:piece :knight
                    :to {:coordinates [5 2]}})
                  [6 0])))}
  [game {:keys [piece to] :as move}]
  (let [turn (state/get-player-in-turn game)]
    (->> state/all-squares
         (map (fn [s] {:square s :piece (state/get-piece game s)}))
         (filter (partial matching-piece? turn piece))
         (filter (fn [s] (rules/valid-move? game (:square s) (:coordinates to))))
         (map :square)
         (disambiguate (:from move))))) ;;TODO: handle errors

(defn- add-castle-coordinates
  "Add the KING's to and from coordinates for a castling move"
  {:test (fn []
           (is (= (add-castle-coordinates {:castles :KINGSIDE} :white)
                  {:from {:coordinates [4 0]}
                   :to {:coordinates [6 0]}
                   :castles :KINGSIDE})))}
  [{:keys [castles] :as move} color]
  (let [rank {:white 0 :black 7}
        file {:KINGSIDE 6 :QUEENSIDE 2}
        start-file 4]
    (merge move
           {:from {:coordinates [start-file (color rank)]}
            :to {:coordinates [(castles file) (color rank)]}})))

(defn- add-coordinates
  "Add the coordinates of the destination square to the move map"
  {:test (fn[]
           (is (= (add-coordinates
                   state/new-game
                   {:piece :pawn
                    :to {:file :d :rank :4}})
                  {:piece :pawn
                   :to {:file :d :rank :4 :coordinates [3 3]}
                   :from {:coordinates [3 1]}})))}
  [game move]
  (if (contains? move :castles)
    (add-castle-coordinates move (state/get-player-in-turn game))
    (as-> move m
      (assoc-in m [:to :coordinates] (coordinates-to m))
      (assoc-in m [:from :coordinates] (coordinates-from game m)))))

(defn- process-move
  "Play the move and add the game state to the map"
  {:test (fn []
           (let [result (process-move
                         tests/promotion-endgame
                         {:move {:piece :pawn
                                 :to {:file :e :rank :8}
                                 :promotion :queen}})]
             (is (= (-> result
                        :state
                        (state/get-piece [4 7])
                        :type)
                    :queen))))}
  [game move-data]
  (let [move (add-coordinates game (:move move-data))
        state (if (contains? move :promotion)
                (rules/move game
                            (get-in move [:from :coordinates])
                            (get-in move [:to :coordinates])
                            (get move :promotion))
                (rules/move game
                            (get-in move [:from :coordinates])
                            (get-in move [:to :coordinates])))]
    {:move move
     :state state}))


(defn- process-move-pair
  "Update both moves with their coordinates and resulting game state"
  {:test (fn []
           (let [result (process-move-pair state/new-game
                                           {:white
                                            {:move {:piece :pawn
                                                    :to {:file :e :rank :4}}}
                                            :black {:move {:piece :knight
                                                           :to {:file :c :rank :6}}}})]
             (is (= (-> result
                        :black
                        :state
                        (state/get-piece [2 5]))
                    {:type :knight
                     :color :black
                     :moved? true})))
           )}
  [game {:keys [black white]}]
  (let [updated-white (process-move game white)]
    {:white updated-white
     :black (process-move (:state updated-white) black)}))

(defn add-game-state-to-move-list
  "Add coorinates and game state to a list of move pairs"
  {:test (fn []
           (let [result (add-game-state-to-move-list [{:white
                                                       {:move {:piece :pawn
                                                               :to {:file :e :rank :4}}}
                                                       :black {:move {:piece :knight
                                                                      :to {:file :c :rank :6}}}}])]
             (is (= (-> result
                        first
                        :black
                        :state
                        (state/get-piece [4 3]))
                    {:type :pawn
                     :color :white
                     :moved? true})))
           )}
  [moves]
  (loop [game-state state/new-game
         to-process moves
         processed []]
    (if (empty? to-process)
      processed
      (let [move-pair (->> to-process
                           first
                           (process-move-pair game-state))]
        (recur (get-in move-pair [:black :state])
               (rest to-process)
               (conj processed move-pair))))))
