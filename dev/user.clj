(ns user)

(ns clojure.core)

(defmacro def-let
  "like let, but binds the expressions globally."
  [bindings & more]
  (let [let-expr (macroexpand `(let ~bindings))
        names-values (partition 2 (second let-expr))
        defs   (map #(cons 'def %) names-values)]
    (concat (list 'do) defs more)))

(defn pp [x]
  ((resolve 'clojure.pprint/pprint) x)
  x)
