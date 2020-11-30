(ns chess-stats.util.notation
  "Everything related to Chess notation."
  (:require [clojure.test :refer [is]]
            [clojure.string :as str]))

(def piece-lookup {"R" :rook
                   "N" :knight
                   "B" :bishop
                   "Q" :queen
                   "K" :king})

(defn- chars->square
  "For a character sequence representing a rank, file, or square return a map of all available info"
  {:test (fn[]
           (is (= (chars->square []) nil))
           (is (= (chars->square ["d"])
                  {:file :d}))
           (is (= (chars->square ["1"])
                  {:rank :1}))
           (is (= (chars->square ["a" "1"])
                  {:file :a :rank :1})))}
  [chars]
  (case (count chars)
    0 nil
    1 (let [c (first chars)
            key (if (try (Integer/parseInt c)
                         (catch Exception e nil))
                  :rank
                  :file)]
        {key (keyword c)})
    2 {:file (keyword (first chars))
       :rank (keyword (last chars))}))

(defn- piece-moved
  "Returns keyword of the piece moved"
  {:test (fn[]
           (is (= (piece-moved ["N" "x" "d" "4"])
                  :knight))
           (is (= (piece-moved ["e" "x" "d" "5"])
                  :pawn))
           (is (= (piece-moved ["e" "4"])
                  :pawn)))}
  [move-chars]
  (let [first-char (first move-chars)]
    (if (= first-char (str/upper-case first-char))
      (get piece-lookup first-char)
      :pawn)))

(defn- get-pawn-origin-and-destination-square
  "From the notation characters, return the pawns origin file if it's a capture"
  {:test (fn[]
           (is (= (get-pawn-origin-and-destination-square ["e" "4"])
                  {:to {:file :e :rank :4}}))
           (is (= (get-pawn-origin-and-destination-square ["e" "x" "d" "4"])
                  {:from {:file :e}
                   :to {:file :d :rank :4}})))}
  [move-str-list]
  (-> (if (some #(= "x" %) move-str-list)
        {:from {:file (-> move-str-list
                first
                keyword)}}
        {})
      (assoc :to (chars->square (take-last 2 move-str-list)))))

(defn- get-piece-origin-and-destination-square
  "From notation characters, find disambiguating origin square information if present"
  [move-str-list]
  (let [location-chars (filterv #(and (not= % "+")
                                      (not= % "#")
                                      (not= % "x")
                                      (not (contains? piece-lookup %))
                                      )
                                move-str-list)
        origin-chars (drop-last 2 location-chars)
        dest-chars (take-last 2 location-chars)]
    (->
        (if (empty? origin-chars)
          {}
          {:from (chars->square origin-chars)})
        (assoc :to (chars->square dest-chars)))))

(defn- piece-moved-with-squares
  "For the given move, return both the piece moved and origin rank and file if given"
  {:test (fn[]
           (is (= (piece-moved-with-squares "e4")
                  {:piece :pawn
                   :to {:file :e :rank :4}}))
           (is (= (piece-moved-with-squares "R1d2")
                  {:piece :rook
                   :from {:rank :1}
                   :to {:file :d :rank :2}}))
           (is (= (piece-moved-with-squares "Nf3xd5")
                  {:piece :knight
                   :from {:file :f
                          :rank :3}
                   :to {:file :d :rank :5}}))
           (is (= (piece-moved-with-squares "exd5")
                  {:piece :pawn
                   :from {:file :e}
                   :to {:file :d :rank :5}})))}
  [move-string]
  (let [move-chars (str/split move-string #"")
        piece (piece-moved move-chars)]
    (if (= piece :pawn)
      (merge {:piece :pawn}
             (get-pawn-origin-and-destination-square move-chars))
      (merge {:piece piece}
             (get-piece-origin-and-destination-square move-chars)))))

(defn string->move
  "Convert a notation string to a map of the contents"
  {:test (fn[]
           (is (= (string->move "exf8=Q")
                  {:promotion :queen
                   :piece :pawn
                   :from {:file :e}
                   :to {:file :f :rank :8}}))
           (is (= (string->move "Nf3xd5")
                  {:piece :knight
                   :from {:file :f
                          :rank :3}
                   :to {:file :d :rank :5}})))}
  [notation-string]
  (let [[move-meta notation-string] (if (str/includes? notation-string "=")
                                     [{:promotion (->> notation-string
                                                      last
                                                      str
                                                      (get piece-lookup))}
                                      (->> notation-string
                                           (drop-last 2)
                                           (apply str))]
                                     [{} notation-string])]
    (-> notation-string
        piece-moved-with-squares
        (merge move-meta))))
