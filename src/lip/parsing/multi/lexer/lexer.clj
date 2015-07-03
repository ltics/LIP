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

(defn consume
  [{:keys [_ point input]}]
  (let [point (inc point)
        char (if (>= point (count input))
               EOF
               (.charAt input point))]
    (get-lex-map char point input)))

(consume {:char \space, :point 7, :input "[abc, b ]"})

(defn advance
  []
  )

(defn match
  [x lex-map]
  (let [char (:char lex-map)]
    (if (= x char)
      (consume lex-map)
      (throw (Error. (str "expecting " x "; found " char))))))

(defprotocol Lexer
  (next-token [lexer])
  (get-token-name [lexer token-type]))