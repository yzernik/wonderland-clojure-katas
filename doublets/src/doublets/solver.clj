(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn diff-chars [word1 word2]
  (count (filter identity (map not= word1 word2))))

(defn neighbor? [word1 word2]
  (if (= (count word1) (count word2))
    (= 1 (diff-chars word1 word2))))

(defn neighbors [word]
  (set (filter (partial neighbor? word) words)))

(defn search [goal cur seen path]
  (cond (= goal cur) path
        :else (let [options (clojure.set/difference (neighbors cur) seen)]
                (cond (and (empty? options) (empty? path))
                      nil
                      (empty? options)
                      (recur goal (first path) seen (rest path))
                      :else
                      (let [choice (first options)]
                        (recur goal choice (conj seen choice) (conj path cur)))))))

(defn doublets [word1 word2]
  (let [res (search word2 word1 #{word1} nil)]
    (cond res (reverse (conj res word2))
          :else [])))
