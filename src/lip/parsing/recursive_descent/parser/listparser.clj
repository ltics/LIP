(ns lip.parsing.recursive-descent.parser.listparser
  (:use [lip.parsing.recursive-descent.parser.parser]
        [lip.parsing.recursive-descent.lexer.token]
        [lip.parsing.recursive-descent.lexer.lexer :exclude [match consume]]
        [lip.parsing.recursive-descent.lexer.listlexer]))

(declare parse-list)
(declare parse-elements)
(declare parse-element)

(defrecord ListParser [input lookahead] Parser)

(defn match-list
  [parser token-type]
  (->ListParser (next-token (:input parser)) (match parser token-type)))

(defn parse-list
  [parser]
  (match-list
    (parse-elements
      (match-list parser LBRACK)) RBRACK))

(defn parse-elements
  [parser]
  (let [parser-atom (atom (parse-element parser))]
    (while (= (:type (:lookahead @parser-atom)) COMMA)
      (reset! parser-atom (parse-element (match-list @parser-atom COMMA))))
    @parser-atom))

(defn parse-element
  [parser]
  (let [lookahead (:lookahead parser)
        type (:type lookahead)]
    (condp = type
      NAME (match-list parser NAME)
      LBRACK (parse-list parser)
      (throw (Error. (str "expecting name or list; found " (to-string lookahead)))))))

(defn construct-listparser
  [lexer]
  (let [next-lexer (next-token lexer)]
    (->ListParser next-lexer (:token next-lexer))))

(comment
  ;;CompilerException java.lang.Error: expecting name or list; found <']',RBRACK>
  (let [lexer (construct-listlexer "[a, ]")
        parser (construct-listparser lexer)]
    (parse-list parser)))