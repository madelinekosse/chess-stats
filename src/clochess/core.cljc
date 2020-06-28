(ns clochess.core
  (:require [clochess.construct :refer :all]
            [clochess.debug :refer :all]))

(defn file-rank-to-string
  [file rank]
  (str (char (+ 97 file)) (inc rank)))

(defn out-of-bounds?
  [file rank]
  (not (and (<= 0 file 7)
            (<= 0 rank 7))))

(defn free?
  [state file rank]
  (= (get-piece state file rank) nil))

(defn friendly?
  [state file rank color]
  (let [piece (get-piece state file rank)]
    (= (:color piece) color)))

(defn enemy?
  [state file rank color]
  (and (not (friendly? state file rank color))))

(defn remove-blocked
  [state color coords]
  (let [free-fn           (fn [[file rank]] (free? state file rank))
        [free not-free]   (split-with free-fn coords)
        [file rank]       (first not-free)
        capture-possible? (and (not (nil? file))
                               (not (nil? rank))
                               (enemy? state file rank color))]
    (if capture-possible?
      (conj free [file rank])
      free)))

(defn valid-moves-king
  [state file rank color]
  (remove (fn [[file rank]] (friendly? state file rank color))
          '([(- file 1) (- rank 1)]
            [file       (- rank 1)]
            [(+ file 1) (- rank 1)]
            [(- file 1) rank]
            [(+ file 1) rank]
            [(- file 1) (+ rank 1)]
            [file       (+ rank 1)]
            [(+ file 1) (+ rank 1)])))

(defn valid-moves-queen
  [state file rank color]
  (println "Queen movement not yet implemented"))

(defn valid-moves-rook
  [state file rank color]
  (let [north (map vector (repeat file) (range (inc rank) 8))
        south (map vector (repeat file) (range (dec rank) -1 -1))
        east  (map vector (range (inc file) 8) (repeat rank))
        west  (map vector (range (dec file) -1 -1) (repeat rank))]
    (concat (remove-blocked state color north)
            (remove-blocked state color south)
            (remove-blocked state color east)
            (remove-blocked state color west))))

(defn valid-moves-bishop
  [state file rank color]
  (let [nw  (map vector (range (dec file) -1 -1) (range (inc rank) 8))
        sw  (map vector (range (dec file) -1 -1) (range (dec file) -1 -1))
        ne  (map vector (range (inc file) 8) (range (inc rank) 8 ))
        se  (map vector (range (inc file) 8) (range (dec file) -1 -1))]
    (concat (remove-blocked state color nw)
            (remove-blocked state color ne)
            (remove-blocked state color sw)
            (remove-blocked state color se))))

(defn valid-moves-queen
  [state file rank color]
  (concat (valid-moves-rook state file rank color)
          (valid-moves-bishop state file rank color)))

(defn valid-moves-knight
  [state file rank color]
  (println "Knight movement not yet implemented"))

(defn valid-moves-pawn
  [state file rank color]
  (println "Pawn movement not yet implemented"))

(defn valid-moves
  [state file rank]
  (let [piece      (get-piece state file rank)
        color      (:color piece)
        type       (:type piece)
        type-moves {:king   valid-moves-king
                    :queen  valid-moves-queen
                    :rook   valid-moves-rook
                    :bishop valid-moves-bishop
                    :knight valid-moves-knight
                    :pawn   valid-moves-pawn
                    nil     (fn [& _] '())}
        moves-fn   (get type-moves type)]
    (moves-fn state file rank color)))
