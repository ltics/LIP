(ns lip.parsing.multi.lexer-test
  (:use [lip.parsing.multi.lexer.lexer]
        [lip.parsing.multi.lexer.lookaheadlexer])
  (:require [clojure.test :refer :all]))

(deftest lexer
  (testing "cosume character"
    (is (= (consume (init-lexer "  hehe 333") ignore-whitespace)
           {:char \h, :point 2, :input "  hehe 333"}))))

(deftest listlexer
  (testing "get-nameseq"
    (is (= (get-nameseq {:char \h, :point 0, :input "hehe 333"})
           {:token   (->ListToken NAME "hehe")
            :lex-map {:char \3, :point 5, :input "hehe 333"}}))
    (is (= (get-nameseq {:char \a, :point 1, :input "[a, b ]"})
           {:token   (->ListToken NAME "a")
            :lex-map {:char \,, :point 2, :input "[a, b ]"}}))
    (is (= (get-nameseq {:char \a, :point 1, :input "[abc, b ]"})
           {:token   (->ListToken NAME "abc")
            :lex-map {:char \,, :point 4, :input "[abc, b ]"}})))
  (testing "is whitespace"
    (is (false? (is-whitespace \a)))
    (is (true? (is-whitespace \space))))
  (testing "is letter"
    (is (true? (is-letter {:char \a, :point 1, :input "[a, b ]"}))))
  (testing "ignore whitespace"
    (is (= (ignore-whitespace {:char \space, :point 0, :input "    333"})
           {:char \3, :point 4, :input "    333"})))
  (testing "get next token"
    (let [lexer (construct-lookaheadlexer "[a, b ]")]
      (is (= (next-token lexer)
             (->LookaheadLexer (->ListToken LBRACK "[")
                               {:char \a, :point 1, :input "[a, b ]"})))
      (is (= (next-token (next-token lexer))
             (->LookaheadLexer (->ListToken NAME "a")
                               {:char \,, :point 2, :input "[a, b ]"})))
      (is (= (next-token (next-token (next-token lexer)))
             (->LookaheadLexer (->ListToken COMMA ",")
                               {:char \b, :point 4, :input "[a, b ]"})))))
  (testing "parse list input"
    (lexer-list "[a, b=c, [d, e]]")
    (prn (clojure.string/join (repeat 15 "-")))
    (lexer-list "[aaa, [bbb, ccc], ddd, mememe]")
    (prn (clojure.string/join (repeat 15 "-")))
    (lexer-list "[a,b=c,[d,e]]")))
