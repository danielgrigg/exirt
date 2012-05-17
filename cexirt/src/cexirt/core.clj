(ns cexirt.core
  (:gen-class)
  (:require [incanter [core :as icc]]) 
  (:require [clojure.math.numeric-tower :as math]))

;; Note we use defrecord liberally to ease development, though at 'some point',
;; many should be replaced with deftypes.

 (set! *warn-on-reflection* true)
; (set! *unchecked-math* true)

(def ^:const pi 3.141592653589793)
(def ^:const epsilon 4e-5)
(def ^:const infinity 1e37)

(defmacro bench [n & exprs]
  `(time
   (dotimes [~'_ ~n]
     (do ~@exprs))))

(defmacro jna-call [lib func ret & args] 
  `(let [library#  (name ~lib)
           function# (com.sun.jna.Function/getFunction library# ~func)] 
           (.invoke function# ~ret (to-array [~@args]))))

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

;; (defn dot [a b] (icc/sum (icc/mult a b)))
      
(defn dot [a b]
  (+ (* (a 0) (b 0)) (* (a 1) (b 1)) (* (a 2) (b 2))))

(defn my-sq [a]
  (* a a))

(defn cross [u v]
  (let [a #^vec3 (apply ->vec3 u)
        b #^vec3 (apply ->vec3 v)]
    (icc/matrix [(- (* (.y a) (.z b)) (* (.z a) (.y b)))
             (- (* (.z a) (.x b)) (* (.x a) (.z b)))
             (- (* (.x a) (.y b)) (* (.y a) (.x b)))])))

(deftype Ray [origin direction mint maxt])

(defn new-ray [origin direction]
  (Ray. origin direction epsilon infinity))

(defprotocol RayIntersections
  "Ray to shape intersection methods"
;;  (intersect [shape  r] "compute the intersection")
  (intersectP [shape  r] "test intersection exists"))

; centre only exists until transforms are supported.
(deftype Sphere [radius])

;;         RayIntersections
         (defn intersect [^Sphere shape ^Ray ray] ; {:hit false :t 99.9})
           (let [A (dot (.direction ray) (.direction ray))
                 B (* 2.0 (dot (.direction ray) (.origin ray)))
                 C (- (dot (.origin ray) (.origin ray)) (my-sq (.radius shape) ))]
             (if-let [t (quadratic A B C)]
               (let [t0 (first t)
                     t1 (second t)]
                 (if-not (or (> t0 (.maxt ray)) (< t1 (.mint ray)))
                   (if (< t0 (.mint ray))
                     [ (> (.maxt ray) t1) t1]
                     [ true t0]))))))
                 
 ;;        (intersectP [shape r ] false))


 
(defn new-sphere [r] (Sphere. r))

(defn squash-twice [coll]
  (mapcat identity (mapcat identity coll)))

(defn noise-framebuffer [w h]
      (repeatedly (* w h) #(vector (rand) (rand) (rand) 1.0)))

(defn native-framebuffer [fb]
;;      (float-array (mapcat identity fb)))
        (float-array (reduce #(apply conj % %2) [] fb)))

(defn write-noisy [w h]
  (jna-call :exr_basic "write_rgba" Integer w h
            (native-framebuffer
             (noise-framebuffer w h))))

(defn mtest []
  (let [s (new-sphere 200)
        r (range -300 300 1)
        l (count r)]
    (jna-call :exr_basic "write_rgba" Integer l l
              (native-framebuffer 
                (map #(if (first %) [1 1 0 1] [0 0 0 1])
                     (for [x r y r]
                          (intersect s (new-ray [x y 4] [0 0 -1]))))))))

(defn mtest2 []
 (let [s (new-sphere 100)
       r (range -200 200 1)
       l (count r)]
   (doall
        (for [x r y r]
          (intersect s (new-ray [x y 4] [0 0 -1]))))))
 
(defn mtest3 []
 (let [s (new-sphere 200)
        r (range -300 300 1)
       l (count r)]
   (dotimes [n (* 300 300)]
   
     (intersect s (new-ray [0 0 4] [0 0 -1])))))


(defn mtest4 []
  (for [n (range (* 600 600))]
    (let [^Sphere s (new-sphere 200)
          ^Ray ray (new-ray [0 0 4] [0 0 -1])
          A (dot (.direction ray) (.direction ray))
          B (* 2.0 (dot (.direction ray) (.origin ray)))
          C (- (dot (.origin ray) (.origin ray)) (my-sq (.radius s)))]

        (quadratic A B C))))

(comment (time (reduce + 0 (map first (mtest4)))))
     
(defn -main [& args]
      (println "Running mtest")
      (mtest)
;;      (time (reduce + 0 (map first (mtest4))))
;;      (bench 1 (mtest2))
      (println "Done."))

 ;;   (println (apply str (interpose "\n" (map #(apply str %) M))))))

