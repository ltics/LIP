(ns lip.parsing.token)

(defprotocol Token
  (to-string [token]))