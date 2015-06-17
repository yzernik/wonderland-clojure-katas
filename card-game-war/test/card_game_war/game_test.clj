(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))

(defn beats [card1 card2]
  (is (play-round card1 card2))
  (is (not (play-round card2 card1))))

;; fill in  tests for your game
(deftest test-play-round
  (testing "the highest rank wins the cards in the round"
    (doseq [[r1 r2] (partition 2 1 ranks)
            s1 suits
            s2 suits]
      (beats [s1 r2] [s2 r1])))
  (testing "queens are higher rank than jacks"
    (doseq [s1 suits
            s2 suits]
      (beats [s1 :queen] [s2 :jack])))
  (testing "kings are higher rank than queens"
    (doseq [s1 suits
            s2 suits]
      (beats [s1 :king] [s2 :queen])))
  (testing "aces are higher rank than kings"
    (doseq [s1 suits
            s2 suits]
      (beats [s1 :ace] [s2 :king])))
  (testing "if the ranks are equal, clubs beat spades"
    (doseq [r ranks]
      (beats [:club r] [:spade r])))
  (testing "if the ranks are equal, diamonds beat clubs"
    (doseq [r ranks]
      (beats [:diamond r] [:club r])))
  (testing "if the ranks are equal, hearts beat diamonds"
    (doseq [r ranks]
      (beats [:heart r] [:diamond r]))))

(deftest test-play-game
  (testing "the player loses when they run out of cards"
    (is (not (play-game nil '([:spade 5]))))
    (is (play-game '([:spade 5]) nil))
    (is (play-game '([:spade :queen]) '([:heart 2]
                                        [:heart 3]
                                        [:heart 4]
                                        [:heart 5]
                                        [:heart 6]
                                        [:heart 7])))))
