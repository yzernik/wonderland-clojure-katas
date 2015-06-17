(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(defn suit [card]
  (get card 0))

(defn rank [card]
  (get card 1))

(defn make-comparator [prop order]
  (let [get-pos #(.indexOf order (prop %))]
    (fn [c1 c2]
      (- (get-pos c1)
         (get-pos c2)))))

(def compare-suits
  (make-comparator suit suits))

(def compare-ranks
  (make-comparator rank ranks))

(defn play-round [c1 c2]
  (let [rank-diff (compare-ranks c1 c2)
        suit-diff (compare-suits c1 c2)]
    (or (> rank-diff 0)
        (and (= rank-diff 0)
             (> suit-diff 0)))))

(defn play-game [cards1 cards2]
  (cond (empty? cards1) false
        (empty? cards2) true
        :else (let [[c1 & hand1] cards1
                    [c2 & hand2] cards2]
                (cond (play-round c1 c2)
                      (recur (into hand1 [c1 c2]) hand2)
                      :else
                      (recur hand1 (into hand2 [c1 c2]))))))
