;; Magnus - a Clojure chess library
;; Copyright (C) 2020  Anders Eriksson

;; This file is part of Magnus.

;; Magnus is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Magnus is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with Magnus.  If not, see <https://www.gnu.org/licenses/>.

(ns magnus.core
  "Core functionality for playing Chess."
  (:require [clojure.test :refer [is]]
            [magnus.construct :refer [all-squares
                                        clear-square
                                        get-piece
                                        new-blank-game
                                        new-game
                                        new-piece
                                        set-moved
                                        set-piece]]
            [magnus.util :refer [in?]]))

(defn manhattan-distance
  "Manhattan distance between two squares."
  {:test (fn []
           (is (= (manhattan-distance [0 0] [0 0])
                  0))
           (is (= (manhattan-distance [0 0] [4 2])
                  (manhattan-distance [4 2] [0 0])
                  6)))}
  [a b]
  (apply + (map #(Math/abs (- %1 %2)) a b)))

(defn out-of-bounds?
  "True if the given file rank coordinates are outside the chessboard.
   Otherwise false."
  {:test (fn []
           (is (not-any? out-of-bounds? [[1 3] [6 2] [0 0] [7 7]]))
           (is (every? out-of-bounds? [[-1 4] [1 8] [9 9] [-3 -4]])))}
  [[file rank]]
  (not (and (<= 0 file 7)
            (<= 0 rank 7))))

(defn free?
  "True if the given square is not occupied by a piece. Otherwise false."
  {:test (fn []
           (is (and (-> new-game
                        (free? [4 4]))))
           (is (not (-> new-game
                        (free? [0 0])))))}
  [state square]
  (nil? (get-piece state square)))

(defn friendly?
  "True if piece at the given square is of the given color.
   Otherwise false."
  {:test (fn []
           (is (-> new-game
                   (friendly? :white [0 0])))
           (is (-> new-game
                   (friendly? :black [7 7])))
           (is (not (-> new-game
                        (friendly? :black [0 0]))))
           (is (not (-> new-game
                        (friendly? :white [7 7])))))}
  [state color square]
  (-> (get-piece state square)
      (:color)
      (= color)))

(def opposite-color
  "Maps color to opposite color."
  {:white :black
   :black :white})

(defn enemy?
  "True if piece at the given square is of the color
   opposite to the one given.Otherwise false."
  {:test (fn []
           (is (-> new-game
                   (enemy? :black [0 0])))
           (is (-> new-game
                   (enemy? :white [7 7])))
           (is (not (-> new-game
                        (enemy? :white [0 0]))))
           (is (not (-> new-game
                        (enemy? :black [7 7]))))
           (is (not (-> new-game
                        (enemy? :black [4 4])))))}
  [state color square]
  (-> (get-piece state square)
      (:color)
      (= (color opposite-color))))

(defn type?
  "True if piece at the given square is of the given type. Otherwise false."
  {:test (fn []
           (is (-> new-game
                   (type? :king [4 0])))
           (is (not (-> new-game
                        (type? :pawn [0 0])))))}
  [state type square]
  (-> (get-piece state square)
      (:type)
      (= type)))

(defn remove-blocked
  "Takes a list of rank file tuples representing a line of valid moves for a
   bishop, rook, or queen, and cuts it off at the first square with a blocking
   piece.
   If that piece is an enemy piece, it is also included in the returning list"
  {:test (fn []
           (is (-> new-game
                   (remove-blocked :white
                                   [[4 4] [4 5] [4 6] [4 7] [4 8]]))
               [[4 4] [4 5] [4 6] [4 7]])
           (is (-> new-game
                   (remove-blocked :black
                                   [[4 4] [4 5] [4 6] [4 7] [4 8]]))
               [[4 4] [4 5] [4 6]])
           (is (-> new-game
                   (remove-blocked :white
                                   [[4 4] [4 3] [4 2] [4 1] [4 0]]))
               [[4 4] [4 3] [4 2]])
           (is (-> new-game
                   (remove-blocked :black
                                   [[4 4] [4 3] [4 2] [4 1] [4 0]]))
               [[4 4] [4 3] [4 2] [4 1]]))}
  [state color squares]
  (let [[free not-free]   (split-with (partial free? state) squares)
        square            (first not-free)]
    (if (enemy? state color square)
      (conj free square)
      free)))

(def back-rank
  "Maps color to back rank index."
  {:white 0
   :black 7})

(def pawn-rank
  "Maps color to pawn rank index."
  {:white 1
   :black 6})

(defn castle-available?
  "True if neither king, nor castle on given side, of 
   player in turn has moved. Otherwise false."
  [state side]
  (let [player-in-turn (:player-in-turn state)
        back-rank      (player-in-turn back-rank)
        rook-file      (if (= side :queenside) 0 7)
        king           (get-piece state [4 back-rank])
        rook           (get-piece state [rook-file back-rank])]
    (not-any? #(or (nil? %)
                   (:moved? %))
              [king rook])))

(declare castle?)
(defn valid-moves-king
  "Returns a list of rank-file tuples representing
   valid moves for a king at file and rank"
  {:test (fn []
           (is (-> new-game
                   (valid-moves-king :white [4 4])
                   (count))
               8)
           (is (-> new-game
                   (valid-moves-king :white [0 0])
                   (empty?)))
           (is (= (set (-> new-game
                           (valid-moves-king :white [4 4])))
                  (set [[3 5] [4 5] [5 5]
                        [3 4] [5 4]
                        [3 3] [4 3] [5 3]])))
           (is (= (set (-> new-game
                           (valid-moves-king :white [4 5])))
                  (set [[3 6] [4 6] [5 6]
                        [3 5] [5 5]
                        [3 4] [4 4] [5 4]])))
           (is (= (set (-> new-game
                           (valid-moves-king :black [4 5])))
                  (set [[3 5] [5 5]
                        [3 4] [4 4] [5 4]])))
           (is (in? [2 0]
                    (-> new-blank-game
                        (set-piece (new-piece :king :white) [4 0])
                        (set-piece (new-piece :rook :white) [0 0])
                        (valid-moves-king :white [4 0])))))}
  [state color [file rank]]
  (let [surrounding [[(dec file) (inc rank)]
                     [(dec file) rank]
                     [(dec file) (dec rank)]
                     [file       (inc rank)]
                     [file       (dec rank)]
                     [(inc file) (inc rank)]
                     [(inc file) rank]
                     [(inc file) (dec rank)]]
        back-rank    ((:player-in-turn state) back-rank)]
    (as-> (remove (partial friendly? state color) surrounding) $
      (remove out-of-bounds? $)
      (if (castle? state :queenside)
        (conj $ [2 back-rank])
        $)
      (if (castle? state :kingside)
        (conj $ [6 back-rank])
        $))))

(defn valid-moves-rook
  "Returns a list of rank-file tuples representing
   valid moves for a rook at file and rank"
  {:test (fn []
           (is (-> new-game
                   (valid-moves-rook :white [0 0])
                   (empty?)))
           (is (= (set (-> new-game
                           (valid-moves-rook :white [4 4])))
                  (set [[0 4] [1 4] [2 4] [3 4] [5 4] [6 4]
                        [7 4] [4 3] [4 2] [4 5] [4 6]])))
           (is (= (set (-> new-game
                           (valid-moves-rook :black [4 4])))
                  (set [[0 4] [1 4] [2 4] [3 4] [5 4] [6 4]
                        [7 4] [4 1] [4 3] [4 2] [4 5]]))))}
  [state color [file rank]]
  (let [north (map vector (repeat file) (range (inc rank) 8))
        south (map vector (repeat file) (range (dec rank) -1 -1))
        east  (map vector (range (inc file) 8) (repeat rank))
        west  (map vector (range (dec file) -1 -1) (repeat rank))]
    (->> [north south east west]
         (map (partial remove-blocked state color))
         (apply concat))))

(defn valid-moves-bishop
  "Returns a list of rank-file tuples representing
   valid moves for a bishop at file and rank"
  {:test (fn []
           (is (-> new-game
                   (valid-moves-bishop :white [2 0])
                   (empty?)))
           (is (= (set (-> new-game
                           (valid-moves-bishop :white [4 4])))
                  (set [[3 5] [2 6] [5 5] [6 6] [3 3] [2 2] [5 3] [6 2]])))
           (is (= (set (-> new-game
                           (valid-moves-bishop :black [4 4])))
                  (set [[3 5] [5 5] [3 3] [2 2] [1 1] [5 3] [6 2] [7 1]]))))}
  [state color [file rank]]
  (let [nw (map vector (range (dec file) -1 -1) (range (inc rank) 8))
        sw (map vector (range (dec file) -1 -1) (range (dec rank) -1 -1))
        ne (map vector (range (inc file) 8) (range (inc rank) 8))
        se (map vector (range (inc file) 8) (range (dec rank) -1 -1))]
    (->> [nw sw ne se]
         (map (partial remove-blocked state color))
         (apply concat))))

(defn valid-moves-queen
  "Returns a list of rank-file tuples representing
   valid moves for a queen at file and rank"
  {:test (fn []
           (is (-> new-game
                   (valid-moves-queen :white [0 0])
                   (empty?)))
           (is (= (-> new-game
                      (valid-moves-queen :white [4 4])
                      (count))
                  (-> new-game
                      (valid-moves-queen :black [4 4])
                      (count))
                  19)))}
  [state color square]
  (concat (valid-moves-rook state color square)
          (valid-moves-bishop state color square)))

(defn valid-moves-knight
  "Returns a list of rank-file tuples representing
   valid moves for a knight at file and rank"
  {:test (fn []
           (is (= (-> new-game
                      (valid-moves-knight :white [0 0]))
                  '([1 2])))
           (is (= (set (-> new-game
                           (valid-moves-knight :white [4 4])))
                  (set [[3 6] [5 6]
                        [6 5] [6 3]
                        [3 2] [5 2]
                        [2 5] [2 3]])))
           (is (= (set (-> new-game
                           (valid-moves-knight :black [4 4])))
                  (set [[6 5] [6 3]
                        [3 2] [5 2]
                        [2 5] [2 3]]))))}
  [state color [file rank]]
  (let [jumps [[(+ file 2) (+ rank 1)]
               [(+ file 2) (- rank 1)]
               [(- file 2) (+ rank 1)]
               [(- file 2) (- rank 1)]
               [(+ file 1) (+ rank 2)]
               [(- file 1) (+ rank 2)]
               [(+ file 1) (- rank 2)]
               [(- file 1) (- rank 2)]]]
    (->> jumps
         (remove (partial friendly? state color))
         (remove out-of-bounds?))))

(defn valid-moves-pawn
  "Returns a list of rank-file tuples representing
   valid moves for a pawn at file and rank"
  {:test (fn []
           (is (= (set (-> new-game
                           (valid-moves-pawn :white [1 1])))
                  (set [[1 2] [1 3]])))
           (is (= (-> new-game
                      (set-piece (assoc (new-piece :pawn :white)
                                        :moved?
                                        true)
                                 [2 2])
                      (valid-moves-pawn :white [2 2]))
                  '([2 3])))
           (is (= (set (-> new-game
                           (set-piece (new-piece :pawn :black) [2 2])
                           (valid-moves-pawn :white [1 1])))
                  (set [[1 2] [1 3] [2 2]])))
           (is (empty? (-> new-game
                           (set-piece (new-piece :pawn :black) [0 5])
                           (set-piece (new-piece :pawn :black) [1 5])
                           (set-piece (new-piece :pawn :black) [2 5])
                           (valid-moves-pawn :black [1 6]))))
           (is (= (set (-> new-game
                           (assoc :en-passant [2 2])
                           (valid-moves-pawn :white [1 1])))
                  (set [[1 2] [1 3] [2 2]]))))}
  [state color [file rank]]
  (let [moved?     (:moved? (get-piece state [file rank]))
        direction  (color {:white 1
                           :black -1})
        one-step   [[file (+ rank direction)]]
        two-steps  (if (not moved?)
                     [[file (+ rank (* direction 2))]]
                     [])
        forward    (concat one-step two-steps)
        diagonals  [[(dec file) (+ rank direction)]
                    [(inc file) (+ rank direction)]]
        en-passant (:en-passant state)]
    (concat (take-while (partial free? state)
                        forward)
            (filter #(or (enemy? state color %)
                         (= en-passant %))
                    diagonals))))

(declare move->check?)
(defn valid-moves
  "Returns a list of rank-file tuples representing
   valid moves for the piece at file and rank.
   If allow-check is false (default), the player may not
   place own king in check."
  {:test (fn []
           (is (empty? (-> new-game
                           (valid-moves [3 3]))))
           (is (empty? (-> new-blank-game
                           (set-piece (new-piece :king :white) [0 0])
                           (set-piece (new-piece :rook :black) [1 2])
                           (set-piece (new-piece :rook :black) [2 1])
                           (valid-moves [0 0]))))
           (is (empty? (-> new-blank-game
                           (set-piece (new-piece :king :white) [0 0])
                           (set-piece (new-piece :pawn :white) [1 1])
                           (set-piece (new-piece :bishop :black) [3 3])
                           (valid-moves [1 1])))))}
  ([state square]
   (valid-moves state false square))
  ([state allow-check? square]
   (let [{:keys [color type]} (get-piece state square)
         valid-moves-fns      {:king   valid-moves-king
                               :queen  valid-moves-queen
                               :rook   valid-moves-rook
                               :bishop valid-moves-bishop
                               :knight valid-moves-knight
                               :pawn   valid-moves-pawn
                               nil     (fn [& _] '())}
         valid-moves-fn       (get valid-moves-fns type)
         moves (valid-moves-fn state color square)]
     (if allow-check?
       moves
       (remove (partial move->check? state square) moves)))))

(defn under-attack?
  "True if square is under attack by player currently NOT in turn.
   Otherwise false."
  {:test (fn []
           (is (-> new-game
                   (under-attack? [0 5])))
           (is (not (-> new-game
                        (under-attack? [0 0])))))}
  [state square]
  (->> all-squares
       (filter (partial enemy? state (:player-in-turn state)))
       (map (partial valid-moves state true))
       (apply concat)
       (in? square)))

(defn king-position
  "Get rank-file tuple for king of given color's position.
   Returns nil if no king of given color is found."
  {:test (fn []
           (is (= (-> new-game
                      (king-position :white))
                  [4 0])))}
  [state color]
  (->> all-squares
       (filter (partial friendly? state color))
       (filter (partial type? state :king))
       (first)))

(defn check?
  "True if color is in check, otherwise false."
  {:test (fn []
           (is (-> new-blank-game
                   (set-piece (new-piece :king :white) [1 1])
                   (set-piece (new-piece :pawn :black) [2 2])
                   (check? :white)))
           (is (not (-> new-blank-game
                        (set-piece (new-piece :king :white) [1 1])
                        (set-piece (new-piece :bishop :black) [3 3])
                        (check? :black))))
           (is (not (-> new-blank-game
                        (set-piece (new-piece :king :white) [1 1])
                        (set-piece (new-piece :pawn :black) [3 3])
                        (check? :white))))
           (is (not (-> new-blank-game
                        (set-piece (new-piece :king :white) [1 1])
                        (set-piece (new-piece :pawn :black) [3 3])
                        (set-piece (new-piece :bishop :black) [5 5])
                        (check? :white)))))}
  [state color]
  (under-attack? state (king-position state color)))

(defn valid-move?
  "True if a move from the starting square to the target square is valid.
   Otherwise false."
  {:test (fn []
           (is (-> new-game
                   (valid-move? [1 1] [1 2])))
           (is (not (-> new-game
                        (valid-move? [1 1] [1 4])))))}
  [state square target]
  (in? target
       (valid-moves state square)))

(defn castling-move?
  "True if the squares given constitute a castling move for the current
   player in turn. Otherwise false."
  {:test (fn []
           (is (-> new-game
                   (castling-move? [4 0] [2 0])))
           (is (-> new-game
                   (assoc :player-in-turn :black)
                   (castling-move? [4 7] [6 7]))))}
  [state [file rank] [target-file target-rank]]
  (let [back-rank ((:player-in-turn state) back-rank)]
    (and (type? state :king [file rank])
         (= rank target-rank back-rank)
         (= file 4)
         (or (= target-file 2)
             (= target-file 6)))))

(defn castle?
  "True if player in turn can castle on the given side. Otherwise false."
  {:test (fn []
           (is (-> new-blank-game
                   (set-piece (new-piece :king :white) [4 0])
                   (set-piece (new-piece :rook :white) [0 0])
                   (castle? :queenside)))
           (is (not (-> new-blank-game
                        (set-piece (assoc (new-piece :king :white)
                                          :moved?
                                          true)
                                   [4 0])
                        (set-piece (new-piece :rook :white) [0 0])
                        (castle? :queenside)))
               "Cannot castle if availability is false
                (either piece has previously been moved)")
           (is (not (-> new-game
                        (castle? :queenside)))
               "Cannot castle with pieces between king and rook")
           (is (not (-> new-blank-game
                        (set-piece (new-piece :king :white) [4 0])
                        (set-piece (new-piece :rook :white) [0 0])
                        (set-piece (new-piece :rook :black) [4 7])
                        (castle? :queenside)))
               "Cannot castle if king in check")
           (is (not (-> new-blank-game
                        (set-piece (new-piece :king :white) [4 0])
                        (set-piece (new-piece :rook :white) [0 0])
                        (set-piece (new-piece :rook :black) [2 7])
                        (castle? :queenside)))
               "Cannot castle if king must not pass through a
                square that is underattack by enemy pieces"))}
  [state side]
  (let [rank    (if (= (:player-in-turn state) :white) 0 7)
        squares (if (= side :queenside)
                  [[1 rank] [2 rank] [3 rank]]
                  [[5 rank] [6 rank]])]
    (and (castle-available? state side)
         (every? (partial free? state) squares)
         (not (check? state (:player-in-turn state)))
         (not-any? (partial under-attack? state) squares))))

(defn force-move
  "Moves piece at starting square to target square regardless
   of whether the move is valid or not.
   Does not mark piece as moved where regular `move` would."
  {:test (fn []
           (is (= (-> new-game
                      (force-move [1 1] [1 2])
                      (get-piece [1 2])
                      (:type))
                  :pawn))
           (is (nil? (-> new-game
                         (force-move [1 1] [1 2])
                         (get-piece [1 1])
                         (:type))))
           (is (not (-> new-game
                        (force-move [0 0] [0 1])
                        (castle-available? :queenside)))))}
  [state square target]
  (as-> (get-piece state square) $
    (set-piece state $ target)
    (clear-square $ square)))

(defn move
  "Attempt to move piece at starting square to target square.
   Returns state unchanged if move is not valid."
  {:test (fn []
           (is (not (nil? (-> new-blank-game
                              (set-piece (new-piece :king :white) [4 0])
                              (set-piece (new-piece :rook :white) [0 0])
                              (move [4 0] [2 0])
                              (get-piece [3 0]))))
               "Rook should move when king castles")
           (is (-> new-game
                   (move [0 1] [0 3])
                   (:en-passant))
               [0 2])
           (is (-> new-game
                   (move [0 1] [0 2])
                   (get-piece [0 2])
                   :moved?)
               "Pieces should be flagged as moved")
           (is (-> new-blank-game
                   (set-piece (new-piece :pawn :white) [0 6])
                   (move [0 6] [0 7] :queen)
                   (type? :queen [0 7]))
               "Promotion of pawn"))}
  ([state square target promotion]
   (let [player-in-turn     (:player-in-turn state)
         pawn?              (type? state :pawn square)
         [_ target-rank]    target
         opponent-back-rank ((player-in-turn opposite-color) back-rank)]
     (if (and pawn? (= target-rank opponent-back-rank))
       (set-piece (move state square target)
                  (new-piece promotion 
                             (:player-in-turn state))
                  target)
       (move state square target))))
  ([state square target]
   (let [[target-file target-rank] target
         rook-file                 (get {2 0 6 7} target-file)
         rook-square               [rook-file target-rank]
         rook-target-file          (get {2 3 6 5} target-file)
         rook-target               [rook-target-file target-rank]
         pawn?                     (type? state :pawn square)
         player-in-turn            (:player-in-turn state)
         direction                 (player-in-turn {:white 1
                                                    :black -1})
         [file rank]               square
         distance                  (manhattan-distance square target)
         en-passant-square         [file (+ rank direction)]]
     (if (valid-move? state square target)
       (as-> state $
         (if (castling-move? $ square target)
           (-> (force-move $ rook-square rook-target)
               (set-moved rook-target))
           $)
         (if (and pawn? (= distance 2))
           (-> (assoc $ :en-passant en-passant-square)
               (assoc :en-passant-timer 2))
           $)
         (force-move $ square target)
         (set-moved $ target))
       state))))

(defn move->check?
  "True if the given move puts current player in check. Otherwise false."
  {:test (fn []
           (is (-> new-blank-game
                   (set-piece (new-piece :king :white) [0 0])
                   (set-piece (new-piece :bishop :black) [1 2])
                   (move->check? [0 0] [0 1])))
           (is (-> new-blank-game
                   (set-piece (new-piece :king :white) [0 0])
                   (set-piece (new-piece :pawn :white) [1 1])
                   (set-piece (new-piece :bishop :black) [3 3])
                   (move->check? [1 1] [1 2]))))}
  [state square target]
  (-> (force-move state square target)
      (check? (:player-in-turn state))))

(defn end-turn
  "End turn and set player-in-turn to opposite player."
  {:test (fn []
           (is (= (-> new-game
                      (end-turn)
                      (:player-in-turn))
                  :black))
           (nil? (-> new-game
                     (assoc :en-passant [0 3])
                     (end-turn)
                     (end-turn)
                     :en-passant)))}
  [state]
  (as-> (update state :player-in-turn opposite-color) $
    (update $ :en-passant-timer #(max (dec %) 0))
    (if (= (:en-passant-timer $) 0)
      (assoc $ :en-passant nil)
      $)))

(defn move->end-turn
  "Combination of magnus.core/move and magnus.core/end-turn."
  ([state square target promotion]
   (-> (move state square target promotion)
       (end-turn)))
  ([state square target]
   (-> (move state square target)
       (end-turn))))
