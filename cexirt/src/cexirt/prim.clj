(ns cexirt.prim
  (:use cexirt.geom)
  (:use cexirt.transform)
  (:use [clojure.pprint :only [pprint]]))

;(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(import cexirt.transform.Transform)
(import cexirt.geom.BBox)

(deftype Primitive [^Transform transform shape]
  RayIntersection
  (intersect [this _r]
    (let [^Ray r _r]
      (intersect shape (transform-object r (inverse transform)))))

  Bounded
  (bounding-box [this]
    (transform-object (bounding-box shape) transform))
  
  Object
  (toString [this] (str "bounding-box " (bounding-box this) " "
                        transform  " " (class shape) " " shape)))

(defn primitive [^Transform transform shape]
  (Primitive. transform shape))

;; A Group is a primitive aggregate with a common transform
(deftype ListGroup [^Transform transform ^BBox bounds primitives]
  RayIntersection
  (intersect [this _r]
    (let [^Ray r _r]
      0.0))
  Object
  (toString [this] (str transform " " bounds " " primitives)))
  

(defn test-triangles [z0 z1 zs]
  (for [z (range z0 z1 zs)]
    (triangle (point3 -1 -1 z) (point3 1 -1 z) (point3 0 1 z))))
