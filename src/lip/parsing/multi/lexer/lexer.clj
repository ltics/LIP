(ns lip.parsing.multi.lexer.lexer)

(def EOF :EOF)
(def EOF_TYPE 1)

(defn get-lex-map
  [char point input]
  {:char  char
   :point point
   :input input})

(defn init-lexer
  [input]
  (let [point 0
        char (.charAt input 0)]
    (get-lex-map char point input)))

(defn advance
  [{:keys [_ point input]}]
  (let [point (inc point)
        char (if (>= point (count input))
               EOF
               (.charAt input point))]
    (get-lex-map char point input)))

(defn consume
  [lex-map ws]
  (ws (advance lex-map)))

(defn match
  [x lex-map ws]
  (let [char (:char lex-map)]
    (if (= x char)
      (consume lex-map ws)
      (throw (Error. (str "expecting " x "; found " char))))))

(defprotocol Lexer
  (next-token [lexer])
  (get-token-name [lexer token-type]))