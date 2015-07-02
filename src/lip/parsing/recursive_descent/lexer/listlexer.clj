(ns lip.parsing.recursive-descent.lexer.listlexer
  (:use [lip.parsing.recursive-descent.lexer.lexer]
        [lip.parsing.recursive-descent.lexer.token]
        [lip.parsing.recursive-descent.lexer.cota]))

(def NAME 2)
(def COMMA 3)
(def LBRACK 4)
(def RBRACK 5)

(def tokenmap
  (let [tokennames ["n/a" "<EOF>" "NAME" "COMMA" "LBRACK" "RBRACK"]]
    (into {} (map vector (vec (range (count tokennames))) tokennames))))

(defrecord ListToken [type text]
  Token
  (to-string [token]
    (let [type-name (get tokenmap (:type token))]
      (str "<'" (:text token) "'," type-name ">"))))

(def whitespaces
  [\space \t \n \r])

(defn get-digit
  "get digit from single char string"
  [lex-map]
  (int (:char lex-map)))

(defn is-whitespace
  [c]
  (if (some #(= c %) whitespaces) true false))

(defn is-letter
  [lex-map]
  (let [digit (get-digit lex-map)]
    (or (and (>= digit (int \a))
             (<= digit (int \z)))
        (and (>= digit (int \A))
             (<= digit (int \Z))))))

(defn get-nameseq
  [lex-map]
  (let [buf (StringBuilder.)
        lex-map-atom (atom lex-map)]
    (do-while
      (is-letter @lex-map-atom)
      (.append buf (:char @lex-map-atom))
      (reset! lex-map-atom (consume @lex-map-atom)))
    {:token   (->ListToken NAME (.toString buf))
     :lex-map @lex-map-atom}))

(defn ignore-whitespace
  [lex-map]
  (let [lex-map-atom (atom lex-map)]
    (while (is-whitespace (:char @lex-map-atom))
      (reset! lex-map-atom (consume @lex-map-atom)))
    @lex-map-atom))

(defrecord ListLexer []
  Lexer
  ;;这个函数或许可以试试用monad进行简化
  (next-token [this lex-elem]
    (let [lex-map (if (contains? lex-elem :lex-map)
                    (:lex-map lex-elem)
                    lex-elem)
          char (:char lex-map)
          get-token (fn [lex-map index strflag]
                      (let [new-lex-map (consume lex-map)]
                        {:token   (->ListToken index strflag)
                         :lex-map new-lex-map}))]
      (if (not= char EOF)
        (if (is-whitespace char)
          (let [new-lex-map (ignore-whitespace lex-map)]
            (next-token this new-lex-map))
          (condp = char
            \, (get-token lex-map COMMA ",")
            \[ (get-token lex-map LBRACK "[")
            \] (get-token lex-map RBRACK "]")
            (if (is-letter lex-map)
              (get-nameseq lex-map)
              (throw (Error. (str "invalid character: " (:char lex-map)))))))
        {:token   (->ListToken EOF_TYPE "<EOF>")
         :lex-map lex-map})))
  (get-token-name [_ token-type]
    (get tokenmap token-type)))

(defn parse-list-elem
  [lexer elem]
  (let [result-map (next-token lexer elem)
        token (:token result-map)]
    (if (not= (:type token) EOF_TYPE)
      (do
        (prn (to-string token))
        (recur lexer (:lex-map result-map)))
      (prn (to-string token)))))

(defn parse-list
  [input]
  (let [lexer (->ListLexer)
        init (init-lexer input)]
    (parse-list-elem lexer init)))
