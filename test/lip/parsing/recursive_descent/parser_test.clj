(ns lip.parsing.recursive-descent.parser-test
  (:use [lip.parsing.recursive-descent.parser.listparser]
        [lip.parsing.recursive-descent.lexer.listlexer]
        [lip.parsing.recursive-descent.lexer.lexer])
  (:require [clojure.test :refer :all]))

(deftest listparser
  (testing "list parser"
    (let [lexer (construct-listlexer "[a, b ]")
          parser (construct-listparser lexer)]
      (is (= (parse-list parser)
             (let [token (->ListToken EOF_TYPE (get tokenmap EOF_TYPE))]
               (->ListParser (->ListLexer
                               token
                               {:char EOF, :point 7, :input "[a, b ]"})
                             token)))))))
