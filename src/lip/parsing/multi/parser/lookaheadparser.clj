(ns lip.parsing.multi.parser.lookaheadparser
  (:use [lip.parsing.multi.parser.parser]
        [lip.parsing.token]
        [lip.parsing.multi.lexer.lexer :exclude [match consume]]
        [lip.parsing.multi.lexer.lookaheadlexer]))

(declare parse-list)
(declare parse-elements)
(declare parse-element)

(defrecord LookaheadParser [input parse-map] Parser)

(defn match-list
  [parser token-type]
  (let [next (match parser token-type)]
    (->LookaheadParser (:input next) (:parse-map next))))

(defn parse-list
  [parser]
  (match-list
    (parse-elements
      (match-list parser LBRACK)) RBRACK))

(defn parse-elements
  [parser]
  (let [parser-atom (atom (parse-element parser))]
    (while (= (LA 1 (:parse-map @parser-atom)) COMMA)
      (reset! parser-atom (parse-element (match-list @parser-atom COMMA))))
    @parser-atom))

(defn parse-element
  [parser]
  (let [parse-map (:parse-map parser)
        type (LA 1 parse-map)]
    (if (and (= (LA 1 parse-map) NAME)
             (= (LA 2 parse-map) EQUALS))
      (match-list
        (match-list
          (match-list parser NAME) EQUALS) NAME)
      (condp = type
        NAME (match-list parser NAME)
        LBRACK (parse-list parser)
        (throw (Error. (str "expecting name or list; found " (get-token-name (:input parser) (LT {:index 1 :parse-map parse-map})))))))))

(defn construct-lookaheadparser
  [lexer k]
  (let [result (init-parser-map lexer k 0 (vec (range k)))]
    (->LookaheadParser (:input result) (:parse-map result))))

(comment
  ;;CompilerException java.lang.Error: expecting name or list; found , c
  (let [lexer (construct-lookaheadlexer "[a,b=c,,[d,e]]")
        parser (construct-lookaheadparser lexer 2)]
    (parse-list parser)))