(ns cexirt.filters
  (:use cexirt.essentials)
  (:use cexirt.limath)
  (:require [incanter core charts stats]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defmacro continuous "Discrete to continuous" [d] `(+ ~d 0.5))
(defmacro discrete "Continuous to discrete" [c] `(Math/floor ~c))

(definterface FilterOps
  (^double evaluate [^double x ^double y]))

(deftype Filter [^double width f]
  FilterOps
  (evaluate [this x y]
    (f x y)))

(defn box-filter
  ([] (box-filter 1.0))
  ([^double width] (Filter. width (constantly 1.0))))

(defn tent-filter 
  ([]
     (tent-filter 1.0))
  ([^double width]
     (Filter. width
              (fn [^double x ^double y]
                (* (- width (Math/abs x)) (- width (Math/abs y)))))))

(defn- gaussian1 [^double x ^double a ^double exp-w]
  (- (Math/exp (- (* a x x ))) exp-w))
     
(defn gaussian-filter
  ([] (gaussian-filter 2.0 2.0))
  
  ([^double width ^double alpha]
     (let [exp-w (Math/exp (- (* alpha width width)))]
       (Filter. width
                (fn [^double x ^double y]
               (* (gaussian1 x alpha exp-w) (gaussian1 y alpha exp-w)))))))

(defn mitchell1 [^double B ^double C ^double x]
  (let [x (Math/abs (* 2.0 x))]
    (if (< x 1.0)
      (* (+ (* (-  12.0 (* 9.0 B) (* 6.0 C)) x x x)
         (* (+ -18.0 (* 12.0 B) (* 6.0 C)) x x)
         (- 6.0 (* 2.0 B))) (/ 6.0))

      (* (+ (* (- (+ B (* 6.0 C))) x x x)
            (* (+ (* 6.0 B) (* 30.0 C)) x x)
            (* (- (+ (* 12.0 B) (* 48.0 C))) x)
            (+ (* 8.0 B) (* 24.0 C))) (/ 6.0)))))

(defn mitchell-filter
  ([]
     (mitchell-filter 2.0 (/ 3.0) (/ 3.0)))  

  ([^double width ^double B ^double C]
     (Filter. width
              (fn [^double x ^double y]
                (* (mitchell1 B C (/ x width)) (mitchell1 B C (/ y width)))))))
