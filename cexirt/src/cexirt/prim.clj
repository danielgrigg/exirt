(ns cexirt.prim
  (:use cexirt.limath)
  (:use [cexirt.geom :exclude [Sphere Triangle]])
  (:use cexirt.transform)
  (:use cexirt.shading)
  (:use [clojure.pprint :only [pprint]])) 

;(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(import cexirt.transform.Transform)
(import cexirt.geom.BBox)
(import cexirt.geom.Ray)
  
(defprotocol Traceable
  (trace [this r] ))

(deftype TraceSample [ray position normal]
  Object
  (toString [this] (str {:ray ray :position position :normal normal})))

(defn trace-sample
  ([^Ray r]
     (TraceSample. r nil nil))
  ([^Ray r position normal]
     (TraceSample. r position  normal)))

(defn radiance "Compute radiance along r in world-space" [^Ray r world]
  (if-let [e (trace @world r)]
    (shade-normal e)
    [0. 0. 0. ]))

(deftype Primitive [^Transform transform shape]
  Traceable
  (trace [this _r]
    (let [^Ray r _r]
      (if-let [[t p n] (trace shape (transform-object r (inverse transform)))]
        [t (transform-point p transform) (transform-normal n transform)])))

  Bounded
  (bounding-box [this]
    (transform-object (bounding-box shape) transform))
  
  Object
  (toString [this] (str {:bbox (bounding-box this)
                         :transform transform
                         :shape shape})))

(defn primitive [^Transform transform shape]
  (Primitive. transform shape))

(deftype PrimitiveList [^Transform transform ^BBox bounds primitives]
  Traceable
  (trace [this _r]
    (let [^Ray r _r
          ^Ray r-local (transform-object r (inverse transform))
          [tn pn nn] (reduce (fn [[t0 _ _ :as s1] prim]
                               (if-let [s2 (trace prim (ray-interval r-local t0))] s2 s1))
                             [(.maxt r-local) nil nil]
                             primitives)]
      (if pn
        [tn (transform-point pn transform) (transform-normal nn transform)])))
  Object
  (toString [this] (str {:bbox bounds
                         :transform transform
                         :nprimitives (count primitives)})))
;;                    transform " " bounds " " (count primitives) " children")))

(defn primitive-list [^Transform transform primitives]
  (PrimitiveList. transform nil primitives))

(deftype SphereShape [^double radius]
  Traceable
  (trace [this _r]
    (let [^Ray r _r]
      (if-let [t (intersect-sphere-ray radius r)]
        (let [p (ray-at r t)
              n (vsub4 p (point3 0 0 0))]
          [t p n]))))
  Bounded
  (bounding-box [this]
    (bbox (point3 (- radius) (- radius) (- radius))
          (point3 radius radius radius)))

  Object
  (toString [this]  (str {:radius radius})))

(defn sphere-shape [^double radius] (SphereShape. radius))

(deftype TriangleShape [p0 p1 p2]
  Traceable
  (trace [this r]
    (let [^Ray _r r]
      ((intersect-triangle-ray p0 p1 p2 _r)) 0))

  Bounded
  (bounding-box [this]
    (bbox-union (bbox p0 p1) p2))
  Object
  (toString [this] (str p0 " " p1 " " p2)))

(defn triangle-shape [p0 p1 p2] (TriangleShape. p0 p1 02))
    
(defn test-triangles [z0 z1 zs]
  (for [z (range z0 z1 zs)]
    (triangle (point3 -1 -1 z) (point3 1 -1 z) (point3 0 1 z))))
