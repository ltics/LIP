(ns lip.parsing.recursive-descent.lexer.cota)

(defmacro do-while
  [test & body]
  `(loop []
     ~@body
     (when ~test (recur))))
