(ns cexirt.essentials)

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defmacro bench [n & exprs]
  `(time
   (dotimes [~'_ ~n]
     (do ~@exprs))))

(defmacro jna-call "Call func from lib" [lib func ret & args] 
  `(let [library#  (name ~lib)
           function# (com.sun.jna.Function/getFunction library# ~func)] 
           (.invoke function# ~ret (to-array [~@args]))))

(defmacro str-sym "Symbolise a sequence of tokens" [& args] `(symbol (str ~@args)))

(defmacro clamp "Clamp x to [a, b]" [x a b] `(min ~b (max ~x ~a)))