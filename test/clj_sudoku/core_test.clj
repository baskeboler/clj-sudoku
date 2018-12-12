(ns clj-sudoku.core-test
  (:require [clojure.test :refer :all]
            [clj-sudoku.core :refer :all]))


(deftest gen-sudoku
  (testing "Generates correct sudoku boards"
    (let [s (sku)]
      (is s))))
