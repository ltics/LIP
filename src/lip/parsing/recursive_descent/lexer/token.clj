(ns lip.parsing.recursive-descent.lexer.token)

(defprotocol Token
  (to-string [token]))