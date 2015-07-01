(ns lip.parser.lexer-test
  (:use [lip.parser.lexer.lexer]
        [lip.parser.lexer.listlexer])
  (:require [clojure.test :refer :all]))

(deftest lexer
  (testing "cosume character"
    (is (consume (init "hehe 333")) {:char \h :point 0 :input "hehe 333"})))