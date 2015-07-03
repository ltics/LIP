(ns lip.parsing.multi.parser.parser
  (:use [lip.parsing.multi.lexer.lexer :exclude [match consume]]))

(defn get-parse-map
  [k point lookahead]
  {:k         k
   :point     point
   :lookahead lookahead})

(defn consume
  [{:keys [input] {:keys [k point lookahead]} :parse-map}]
  (let [next-lexer (next-token input)]
    {:input     next-lexer
     :parse-map (get-parse-map k (mod (inc point) k) (assoc lookahead point (:token next-lexer)))}))

(defn init-parser-map
  [lexer k point lookahead]
  (loop [k k
         parse-map {:input     lexer
                    :parse-map (get-parse-map k point lookahead)}]
    (if (<= k 0)
      parse-map
      (do (prn parse-map)
          (recur (dec k) (consume parse-map))))))

(defn LT
  [{:keys [index] {:keys [k point lookahead]} :parse-map}]
  (nth lookahead (mod (dec (+ point index)) k)))

(defn LA
  [index parse-map]
  (:type (LT {:index     index
              :parse-map parse-map})))

(defn match
  [parser token-type]
  (let [parse-map (:parse-map parser)
        lexer (:input parser)]
    (prn (str "expected type -> " (get-token-name lexer token-type) " actual type -> " (get-token-name lexer (LA 1 parse-map))))
    (if (= (LA 1 parse-map) token-type)
      (consume parser)
      (throw (Error. (str "expecting " (get-token-name lexer token-type) "; found " (get-token-name lexer (:type (LT {:index 1 :parse-map parse-map})))))))))

(defprotocol Parser)
