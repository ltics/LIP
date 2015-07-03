(ns lip.cota_test
  (:require [clojure.test :refer :all]
            [lip.cota :refer :all]))

(deftest lip-test
  (testing "while macro"
    (let [counter (atom 0)]
      (while (< @counter 3)
        (println @counter)
        (swap! counter inc))))
  (testing "do-white macro"
    (let [counter (atom 0)]
      (do-while (< @counter 3)
                (prn @counter)
                (swap! counter inc)))))
