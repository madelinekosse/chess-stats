(ns clochess.core
    (:require [clochess.construct :refer :all]
              [clochess.debug :refer :all]))

(defn out-of-bounds? [file rank]
    (not (and (<= 0 file 7)
              (<= 0 rank 7))))

(defn is-occupied? [state file rank]
    (not (= (get-piece state file rank) nil)))

(defn set-moved [state file rank]
    (assoc-in state [:board rank file :moved?] true))

(defn valid-moves-king [state file rank]
    '('[(- file 1) (- rank 1)]
      '[file       (- rank 1)]
      '[(+ file 1) (- rank 1)]
      '[(- file 1) rank]
      '[(+ file 1) rank]
      '[(- file 1) (+ rank 1)]
      '[file       (+ rank 1)]
      '[(+ file 1) (+ rank 1)]))

(defn valid-moves-queen [state file rank]
    (println "Queen movement not yet implemented"))

(defn valid-moves-rook [state file rank]
    (concat (map vector (range 0 8) (repeat file))
            (map vector (repeat rank) (range 0 8))))

(defn valid-moves-bishop [state file rank]
    (println "Bishop movement not yet implemented"))

(defn valid-moves-knight [state file rank]
    (println "Knight movement not yet implemented"))

(defn valid-moves-pawn [state file rank]
    (println "Pawn movement not yet implemented"))

;;; DEBUG
(defn -main
    "Debug, remove later"
    [& args]
    (print-board (new-game)))
