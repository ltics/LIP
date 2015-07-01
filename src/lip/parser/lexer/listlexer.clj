(ns lip.parser.lexer.listlexer
  (:use [lip.parser.lexer.lexer]
        [lip.parser.lexer.token]
        [lip.parser.lexer.cota]))

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

(defn next-token
  [lex-map]
  (let [char (:char lex-map)
        get-token (fn [lex-map index strflag]
                    (let [new-lex-map (consume lex-map)]
                      {:token   (->ListToken index strflag)
                       :lex-map new-lex-map}))]
    (if (not= char EOF)
      (if (is-whitespace char)
        (let [new-lex-map (ignore-whitespace lex-map)]
          (next-token new-lex-map))
        (condp = char
          \, (get-token lex-map COMMA ",")
          \[ (get-token lex-map LBRACK "[")
          \] (get-token lex-map RBRACK "]")
          (if (is-letter lex-map)
            (get-nameseq lex-map)
            (throw (Error. (str "invalid character: " (:char lex-map)))))))
      {:token   (->ListToken EOF_TYPE "<EOF>")
       :lex-map lex-map})))