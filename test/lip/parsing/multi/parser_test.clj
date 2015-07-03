(ns lip.parsing.multi.parser-test
  (:use [lip.parsing.multi.parser.lookaheadparser]
        [lip.parsing.multi.lexer.lookaheadlexer]
        [lip.parsing.multi.lexer.lexer])
  (:require [clojure.test :refer :all]))

(deftest listparser
  (testing "list parser bingo"
    (let [lexer (construct-lookaheadlexer "[a,b=c,[d,e]]")
          parser (construct-lookaheadparser lexer 2)]
      (is (= (parse-list parser)
             (let [token (->ListToken EOF_TYPE (get tokenmap EOF_TYPE))]
               (->LookaheadParser (->LookaheadLexer
                                    token
                                    {:char EOF, :point 13, :input "[a,b=c,[d,e]]"})
                                  {:k 2 :point 1 :lookahead [token token]}))))))
  (testing "list parser parse the wrong format list but did not emit a exception"
    (let [lexer (construct-lookaheadlexer "[a,b=c, d,e]]")
          parser (construct-lookaheadparser lexer 2)]
      (is (= (parse-list parser)
             (let [token (->ListToken EOF_TYPE (get tokenmap EOF_TYPE))]
               (->LookaheadParser (->LookaheadLexer
                                    token
                                    {:char EOF, :point 13, :input "[a,b=c, d,e]]"})
                                  {:k 2 :point 1 :lookahead [token (->ListToken RBRACK (get tokenmap RBRACK))]})))))))