(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))

(defn make-card [suit rank]
  {:suit suit :rank rank})

(defn beats [card1 card2]
  (is (play-round card1 card2))
  (is (not (play-round card2 card1))))

;; fill in  tests for your game
(deftest test-play-round
  (testing "the highest rank wins the cards in the round")
  (beats (make-card :diamond :ace)
         (make-card :heart :ace)))

(deftest test-play-round
  (testing "the highest rank wins the cards in the round"
    (doseq [[r1 r2] (partition 2 1 ranks)
            s1 suits
            s2 suits]
      (beats (make-card s1 r2) (make-card s2 r1))))
  (testing "if the ranks are equal, the higher suit wins"
    (doseq [r ranks
            [s1 s2] (partition 2 1 suits)]
      (beats (make-card s1 r) (make-card s2 r)))))

(deftest test-play-game
  (testing "the player loses when they have no cards"
    (let [five-of-spades (make-card :spade 5)]
      (is (not (play-game nil [five-of-spades])))
      (is (play-game [five-of-spades] nil))))
  (testing "the player loses when they have no cards"
    (let [queen-of-diamonds (make-card :diamond :queen)]
      (is (play-game [queen-of-diamonds]
                     (for [i (range 2 9)]
                       (make-card :spade i)))))))
