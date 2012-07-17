(ns cexirt.geom
  (:require [clojure.math.numeric-tower :as numeric])
  (:use cexirt.limath)
  (:use cexirt.transform)
  (:use [clojure.pprint :only [pprint]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defprotocol Transformable
  (transform-object [this T]))


(definterface Bounding
  (^double width [])
  (^double height [])
  (^double depth []))

(deftype BoundingBox2 [^double x0 ^double y0 ^double x1 ^double y1]
  Bounding
  (width [this]
    (- x1 x0))
  (height [ this]
    (- y1 y0))
  (depth [this] 0.0)

  Object
  (toString [this]
   (str "(" x0 " " y0 ") (" x1 " " y1 ")")))

(defn clip-bounding-box2 [^BoundingBox2 this ^BoundingBox2 other]
    (BoundingBox2. (max (.x0 this) (.x0 other))
                   (max (.y0 this) (.y0 other))
                   (min (.x1 this) (.x1 other))
                   (min (.y1 this) (.y1 other))))


(defn bounding-box2 [[x0 y0] [x1 y1]]
  (BoundingBox2. x0 y0 x1 y1))


(deftype BoundingBox3 [^double x0 ^double y0 ^double z0 ^double x1 ^double y1 ^double z1]
  Bounding
  (width [this]
    (- x1 x0))
  (height [ this]
    (- y1 y0))
  (depth [this] (- z1 z0))

  Object
  (toString [this]
    (str "(" x0 " " y0 " " z0 ") (" x1 " " y1 " " z1 ")")))

(defn bounding-box3 [[x0 y0 z0] [x1 y1 z1]]
  (BoundingBox3. x0 y0 z0 x1 y1 z1))

(deftype Ray [origin direction ^double mint ^double maxt]
  Transformable
  (transform-object [this T]
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

(defn ^Ray ray
  ([origin direction]
     (Ray. origin direction eps infinity))
  ([origin direction mint maxt]
     (Ray. origin direction mint maxt)))

;; "Ray to shape intersection methods"
(defprotocol RayIntersection
  ( intersect [this r]);; "Distance to intersection")
  )

(deftype Plane [position normal]
  RayIntersection
  (intersect [this _r]
    (let [^Ray r _r
          rdotn (vdot3 (.direction r) normal)]
      (if (> (numeric/abs rdotn) eps-small)
        (let [t (/ (vdot3 (vsub3 position (.origin r)) normal) rdotn)]
          (if (> (.maxt r) t (.mint r)) t)))))
  Object 
  (toString [this] (str position " " normal)))

(defn plane [position normal] (Plane. position normal))
  
(deftype Sphere [^double radius]
  RayIntersection
  (intersect [this r]
    (let [^Ray _r r
          A (vdot3 (.direction _r) (.direction _r))
          B (* 2.0 (vdot3 (.direction _r) (.origin _r)))
          C (- (vdot3 (.origin _r) (.origin _r)) (sq (.radius this) ))]
      (if-let [t (quadratic A B C)]
        (let [t0 (first t)
              t1 (second t)]
          (if-not (or (> t0 (.maxt _r)) (< t1 (.mint _r)))
            (if (< t0 (.mint _r))
              (if (> (.maxt _r) t1) t1)
              t0))))))
                 
  Object
  (toString [this] (str radius)))

(defn sphere [^double r] (Sphere. r))

(deftype Triangle [p0 p1 p2]
  RayIntersection
  (intersect [this r]
    (let [^Ray _r r
          e1 (vsub3 p1 p0)
          e2 (vsub3 p2 p0)
          pvec (cross (.direction _r) e2)
          tvec (vsub3 (.origin _r) p0)
          qvec (cross tvec e1)
          det (vdot3 e1 pvec)
          u (vdot3 tvec pvec)
          v (vdot3 (.direction _r) qvec)]
;      [e1 e2 pvec tvec qvec det u v]))
      (if (and (> det eps)
               (>= u 0.0) (<= u det)
               (>= v 0.0) (<= (+ u v) det))
        (let [tuv (vdiv3s [(vdot3 e2 qvec) u v] det)]
          ;; We've computed the barycentric coordinates u,v but we don't
          ;; currently use them - just return t.
          (tuv 0)))))
  Object
  (toString [this] (str "<" p0 " " p1 " " p2 ">")))

(defn triangle [p0 p1 p2]
  (Triangle. p0 p1 p2))
    
