(ns cexirt.core
  (:gen-class)
  (:require [incanter [core :as icc]]) 
  (:require [clojure.math.numeric-tower :as math]))

;; Note we use defrecord liberally to ease development, though at 'some point',
;; many should be replaced with deftypes.

; (set! *warn-on-reflection* true)
; (set! *unchecked-math* true)

(def ^:const pi 3.141592653589793)
(def ^:const epsilon 4e-5)
(def ^:const infinity 1e37)

(defmacro bench [& exprs]
  `(time
   (dotimes [~'_ 1E6]
     (do ~@exprs))))

; well, type-hinting the parameters doubled throughput..kinda crazy
; can coerce the expressions to double to improve ~3% but it's fugly.
(defn quadratic [^double A ^double B ^double C]
  (let [discrim (- (* B B) (* 4.0 A C))]
    (if (< discrim 0)
      nil
      (let [rootDiscrim (math/sqrt discrim)
            q (if (< B 0)
                (* -0.5 (- B rootDiscrim))
                (* -0.5 (+ B rootDiscrim)))
            t0 (/ q A)
            t1 (/ C q)]
        (if (> t0 t1)
          [t1 t0]
          [t0 t1])))))
        

; VECTOR OPS
(deftype vec3 [x y z])

(defn dot [a b]
  (icc/sum (icc/mult a b)))
      
(defn cross [u v]
  (let [a (apply ->vec3 u)
        b (apply ->vec3 v)]
    (icc/matrix [(- (* (.y a) (.z b)) (* (.z a) (.y b)))
             (- (* (.z a) (.x b)) (* (.x a) (.z b)))
             (- (* (.x a) (.y b)) (* (.y a) (.x b)))])))

(defrecord Ray [origin direction mint maxt])

(defn new-ray [origin direction]
  (Ray. origin direction epsilon infinity))

(defprotocol RayIntersections
  "Ray to shape intersection methods"
  (intersect [shape r] "compute the intersection")
  (intersectP [shape r] "test intersection exists"))


; centre only exists until transforms are supported.
(defrecord Sphere [radius]
         RayIntersections
         (intersect [shape ray] ; {:hit false :t 99.9})
           (let [A (dot (:direction ray) (:direction ray))
                 B (* 2.0 (dot (:direction ray) (.origin ray)))
                 C (- (dot (.origin ray) (.origin ray)) (* (:radius shape) (:radius shape)))]
             (if-let [t (quadratic A B C)]
               (let [t0 (first t)
                     t1 (second t)]
                 (if-not (or (> t0 (.maxt ray)) (< t1 (.mint ray)))
                   (if (< t0 (.mint ray))
                     {:hit (> (.maxt ray) t1) :t t1}
                     {:hit true :t t0}))))))
                 
         (intersectP [shape r ] false))

 
(defn -main [& args]
  (let [s (Sphere. 2.0)
        M (let [r (range -3 3 0.2)
                l (count r)]
            (partition l 
                       (map #(if (:hit %) "*" "+")
                            (for [x r y r]
                              (intersect s (new-ray [x y 4] [0 0 -1]))))))]
    (println (apply str (interpose "\n" (map #(apply str %) M))))))
