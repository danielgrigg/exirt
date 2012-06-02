(ns cexirt.geom
  (:require [clojure.math.numeric-tower :as math])
  (:use cexirt.limath)
  (:use [clojure.pprint :only [pprint]])
  (:require clojure.string))

(defprotocol Transformable
  (transform [this T]))

(deftype Transform [transform inverse]
  Transformable
  (transform [this T]
    (Transform. (mmul4 (.transform this) (.transform T))
                (mmul4 (.inverse T) (.inverse this))))
    
  Object
  (toString [this]
    (str "\n" (clojure.string/join "\n" (.transform this)) "\n")))

;; Assume Transform constructor is private
(defn equal-transform [^Transform A ^Transform B]
  (mequal4 (.transform A) (.transform B)))

(defn inverse [^Transform T]
  (Transform. (.inverse T) (.transform T)))

(defn matrix-transform [M]
  (Transform. M (minverse4 M)))

(defn identity-transform []
  (matrix-transform (midentity4)))

(defmacro compose [& xs]
  `(matrix-transform (mmul4 ~@(for [x xs] `(.transform ~x)))))

(defn translate [^double tx ^double ty ^double tz]
  (Transform. [[1. 0. 0. tx]
               [0. 1. 0. ty]
               [0. 0. 1. tz]
               [0. 0. 0. 1.]]
              [[1. 0. 0. (- tx)]
               [0. 1. 0. (- ty)]
               [0. 0. 1. (- tz)]
               [0. 0. 0. 1.]]))

(defn scale [^double sx ^double sy ^double sz]
  (Transform. [[sx 0. 0. 0.]
               [0. sy 0. 0.]
               [0. 0. sz 0.]
               [0. 0. 0. 1.]]
              [[(/ sx) 0. 0. 0.]
               [0. (/ sy) 0. 0.]
               [0. 0. (/ sz) 0.]
               [0. 0. 0. 1.]]))
  
(defn rotate-x [^double rads]
  (let [c (Math/cos rads)
        s (Math/sin rads)
        M [[1. 0. 0. 0.]
           [0. c (- s) 0.]
           [0. s c 0.]
           [0. 0. 0. 1.]]]
        (Transform. M (mtranspose4 M))))

(defn rotate-y [^double rads]
  (let [c (Math/cos rads)
        s (Math/sin rads)
        M [[c 0. s 0.]
            [0. 1. 0. 0.]
            [(- s) 0. c 0.]
            [0. 0. 0. 1.]]]
    (Transform. M (mtranspose4 M))))

(defn rotate-z [^double rads]
  (let [c (Math/cos rads)
        s (Math/sin rads)
        M [[c (- s) 0. 0.]
           [s c 0. 0.]
           [0. 0. 1. 0.]
           [0. 0. 0. 1.]]]
    (Transform. M (mtranspose4 M))))

(defn rotate-axis [^double rads axis]
  (let [c (Math/cos rads)
        s (Math/sin rads)
        n (vnormalize3 axis)
        x (double (n 0))
        y (double (n 1))
        z (double (n 2))
        M [[(+ c (* (- 1. c) x x))
            (- (* (- 1. c) x y) (* z s))
            (+ (* (- 1. c) x z) (* y s))
            0.]
           [(+ (* (- 1. c) x y) (* z s))
            (+ c (* (- 1. c) y y))
            (- (* (- 1. c) y z) (* x s))
            0.]
            [(- (* (- 1. c) x z) (* y s))
            (+ (* (- 1. c) y z) (* x s))
            (+ c (* (- 1. c) z z))
            0.]
           [0. 0. 0. 1.]]]
    (Transform. M (mtranspose4 M))))

(defn ortho [l r b t n f]
  (let [M [[(/ 2. (- r l)) 0. 0. (- (/ (+ r l) (- r l)))]
           [0. (/ 2. (- t b)) 0. (- (/ (+ t b) (- t b)))]
           [0. 0. (/ 2. (- f n)) (- (/ (+ f n) (- f n)))]
           [0. 0. 0. 1.]]]
    (Transform. M (mtranspose4 M)))) 

(defn perspective [^double fov_rads ^double aspect ^double n ^double f]
  (let [t (* n (Math/tan (* fov_rads 0.5)))
        b (- t)
        l (* b aspect)
        r (- l)
        M [[(/ (* 2. n) (- r l)) 0. (/ (+ r l) (- r l)) 0.]
           [0. (/ (* 2. n) (- t b)) (/ (+ t b) (- t b)) 0.]
           [0. 0. (- (/ (+ f n) (- f n))) (- (/ (* 2. f n) (- f n)))]
           [0. 0. -1. 0.]]]
    (matrix-transform M)))

(defn point3 [^double x ^double y ^double z]
  [x y z 1.0])

(defn vector3 [^double x ^double y ^double z]
  [x y z 0.0])

(defn normal [^double x ^double y ^double z]
  [x y z 0.0])

(defn transform-point [p ^Transform T ] (mvmul4 (.transform T) p))
(defn transform-vector [v ^Transform T] (mvmul4 (.transform T) v))
(defn transform-normal [n ^Transform T] (mvmul4 (mtranspose4 (.inverse T)) n))

; project homogenous point to r3
(defn project [p]
  (if (zero? (p 3))
    p
    (vmul4s p (/ (p 3)))))

(defn angle-of-view [^double plane-width ^double plane-distance]
  (* 2.0 (Math/atan (/ plane-width (* 2.0 plane-distance)))))

(deftype Ray [origin direction ^double mint ^double maxt]
  
  Transformable
  (transform [this T]
    (Ray. (transform-point (.origin this) T)
          (transform-vector (.direction this) T)
          mint
          maxt))
            
  Object
  (toString [this]
    (apply str
           (.origin this) " "
           (.direction this) " "
           (.mint this) " " 
           (.maxt this) " ")))

(defn ray-at [^Ray r ^double t]
  (vadd4 (.origin r) (vmul4s (.direction r) t)))

(defn new-ray
  ([origin direction]
     (Ray. origin direction eps infinity))
  ([origin direction mint maxt]
     (Ray. origin direction mint maxt)))

(defprotocol RayIntersections
  "Ray to shape intersection methods"
  (intersect [shape r] "compute the intersection")
 ;; (intersectP [shape  r] "test intersection exists")
  )

(defrecord Sphere [radius]
  RayIntersections
  (intersect [shape r]
    (let [A (vdot3 (.direction r) (.direction r))
          B (* 2.0 (vdot3 (.direction r) (.origin r)))
          C (- (vdot3 (.origin r) (.origin r)) (sq (.radius shape) ))]
      (if-let [t (quadratic A B C)]
        (let [t0 (first t)
              t1 (second t)]
          (if-not (or (> t0 (.maxt r)) (< t1 (.mint r)))
            (if (< t0 (.mint r))
              [ (> (.maxt r) t1) t1]
              [ true t0]))))))
                 
 ;;        (intersectP [shape r ] false))
  Object
  (toString [this] (str radius)))


(defn new-sphere [r] (Sphere. r))
