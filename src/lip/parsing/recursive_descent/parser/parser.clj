(ns lip.parsing.recursive-descent.parser.parser
  (:use [lip.parsing.recursive-descent.lexer.lexer :exclude [match consume]]))

(defn consume
  [parser]
  (let [lookahead (:token (next-token (:input parser)))]
    lookahead))

(defn match
  [parser token-type]
  (let [lookahead (:lookahead parser)]
    (if (= (:type lookahead) token-type)
      (consume parser)
      (throw (Error. (str "expecting " (get-token-name (:input parser) token-type)))))))

(defprotocol Parser)
