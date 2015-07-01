(ns lip.parser.lexer.token)

(defprotocol Token
  (to-string [token]))