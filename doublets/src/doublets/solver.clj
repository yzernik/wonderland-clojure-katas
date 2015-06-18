(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn replace-char [word index c]
  (let [get-char #(cond (= index %1) c :else %2)]
    (apply str (map-indexed get-char word))))

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(def alphabet (char-range \a \z))

(defn mutations [word]
  (set
   (for [i (range (count word))
         c alphabet]
     (replace-char word i c))))

(defn neighbors [word]
  (let [muts (disj (mutations word) word)]
    (set (filter #(some #{%} words) muts))))

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
