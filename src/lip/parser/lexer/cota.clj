(ns lip.parser.lexer.cota)

(defmacro do-while
  [test & body]
  `(loop []
     ~@body
     (when ~test (recur))))
