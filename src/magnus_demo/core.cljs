(ns magnus-demo.core
  (:require [reagent.core :as r]
            [reagent.dom :as d]
            [magnus.core :as m.core]
            [magnus.construct :as m.con]
            [magnus.util :refer [in?]]))

;; -------------------------
;; Atoms

(def game-state (r/atom m.con/new-game))
(def valid-moves (r/atom '()))
(def selected-square (r/atom nil))

;; -------------------------
;; Util

(defn transpose-rev-board [board]
  (reverse (apply mapv vector board)))

(defn color&type->svg-src [piece]
  (get-in {:white {:king   "/svg/white-king.svg"
                   :queen  "/svg/white-queen.svg"
                   :bishop "/svg/white-bishop.svg"
                   :knight "/svg/white-knight.svg"
                   :rook   "/svg/white-rook.svg"
                   :pawn   "/svg/white-pawn.svg"}
           :black {:king   "/svg/black-king.svg"
                   :queen  "/svg/black-queen.svg"
                   :bishop "/svg/black-bishop.svg"
                   :knight "/svg/black-knight.svg"
                   :rook   "/svg/black-rook.svg"
                   :pawn   "/svg/black-pawn.svg"}}
          [(:color piece) (:type piece)]))

(defn square-style [square]
  (if (in? square @valid-moves)
    "square-highlighted"
    (if (even? (apply + square))
      "square-dark"
      "square-light")))

;; -------------------------
;; On-clicks

(defn move-piece [square]
  (swap! game-state m.core/move @selected-square square)
  (swap! game-state m.core/end-turn)
  (reset! selected-square nil)
  (reset! valid-moves '()))

(defn select-piece [square]
  (reset! valid-moves (m.core/valid-moves @game-state square))
  (reset! selected-square square))

(defn board-square-on-click [square]
  (if (in? square @valid-moves)
    (move-piece square)
    (select-piece square)))

;; -------------------------
;; Components

(defn piece-icon [piece]
  (if piece
    [:img.piece-icon {:src (color&type->svg-src piece)}]
    nil))

(defn board-square [square]
  [:div.square {:class    (square-style square)
                :on-click #(board-square-on-click square)}
   [piece-icon (m.con/get-piece @game-state square)]])

(defn board []
  [:div.board
   (for [rank (range 7 -1 -1) file (range 8)]
     ^{:key [file rank]} [board-square [file rank]])])

(defn reset-button []
  [:input {:type "button"
           :value "Reset board"
           :on-click #(reset! game-state m.con/new-game)}])

(defn home-page []
  [:div [:h2 "Magnus - Demo"]
   [reset-button]
   "Player in turn: " (:player-in-turn @game-state)
   [board]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
