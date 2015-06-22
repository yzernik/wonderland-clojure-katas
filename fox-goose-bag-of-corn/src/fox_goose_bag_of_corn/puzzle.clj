(ns fox-goose-bag-of-corn.puzzle
  (:require [clojure.set]))

(def start-pos [[:fox :goose :corn :you] [:boat] []])

(def end-pos [[] [:boat] [:fox :goose :corn :you]])

(defn to-canonical [pos]
  (vec (map set pos)))

(defn from-canonical [canonical-pos]
  (vec (map vec canonical-pos)))

(defn boat-unsafe? [loc]
  (and (contains? loc :boat)
       (contains? loc :you)
       (> (count loc) 3)))

(defn goose-unsafe? [loc]
  (and (contains? loc :goose)
       (contains? loc :fox)
       (not (contains? loc :you))))

(defn corn-unsafe? [loc]
  (and (contains? loc :corn)
       (contains? loc :goose)
       (not (contains? loc :you))))

(defn loc-safe? [loc]
  (not (or (boat-unsafe? loc)
           (goose-unsafe? loc)
           (corn-unsafe? loc))))

(defn pos-safe? [locations]
  (every? loc-safe? locations))

(defn candidates-to-move [loc]
  (if (contains? loc :you)
    (let [animals (filter #(and (not= % :boat) (not= % :you)) loc)]
      (conj (map #(set [% :you]) animals) #{:you}))))

(def connections {0 [1]
                  1 [0 2]
                  2 [1]})

(defn neighbors [locations]
  (for [i (range 3)
        j (connections i)
        movers (candidates-to-move (locations i))]
    (let [new-from (clojure.set/difference (locations i) movers)
          new-to (clojure.set/union (locations j) movers)]
      (assoc locations i new-from j new-to))))

(defn safe-neighbors [pos]
  (set (filter pos-safe? (neighbors pos))))

(defn search [goal cur seen path]
  (cond (= goal cur) path
        :else (let [options (clojure.set/difference (safe-neighbors cur) seen)]
                (cond (and (empty? options) (empty? path))
                      nil
                      (empty? options)
                      (recur goal (first path) seen (rest path))
                      :else
                      (let [choice (first options)]
                                                (recur goal choice (conj seen choice) (conj path cur)))))))

(defn canonical-river-crossing-plan []
  (let [start-locations (to-canonical start-pos)
        end-locations (to-canonical end-pos)
        res (search end-locations start-locations #{start-locations} nil)]
    (cond res (reverse (conj res end-locations))
          :else [])))


(defn river-crossing-plan []
  (map from-canonical (canonical-river-crossing-plan)))
