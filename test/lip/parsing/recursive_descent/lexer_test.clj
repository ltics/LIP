(ns lip.parsing.recursive-descent.lexer-test
  (:use [lip.parsing.recursive-descent.lexer.lexer]
        [lip.parsing.recursive-descent.lexer.listlexer])
  (:require [clojure.test :refer :all]))

(deftest lexer
  (testing "cosume character"
    (is (consume (init-lexer "hehe 333")) {:char \h :point 0 :input "hehe 333"})))

(deftest listlexer
  (testing "get-nameseq"
    (is (= (get-nameseq {:char \h, :point 0, :input "hehe 333"})
           {:token   (->ListToken NAME "hehe")
            :lex-map {:char \space, :point 4, :input "hehe 333"}}))
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
    (let [lexer (construct-listlexer "[a, b ]")]
      (is (= (next-token lexer)
             (->ListLexer (->ListToken LBRACK "[")
                          {:char \a, :point 1, :input "[a, b ]"})))
      (is (= (next-token (next-token lexer))
             (->ListLexer (->ListToken NAME "a")
                          {:char \,, :point 2, :input "[a, b ]"})))
      (is (= (next-token (next-token (next-token lexer)))
             (->ListLexer (->ListToken COMMA ",")
                          {:char \space, :point 3, :input "[a, b ]"})))))
  (testing "parse list input"
    (lexer-list "[a, b ]")
    (prn (clojure.string/join (repeat 15 "-")))
    (lexer-list "[aaa, [bbb, ccc], ddd, mememe]")))