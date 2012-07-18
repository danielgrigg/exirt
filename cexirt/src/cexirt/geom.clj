(ns cexirt.geom
  (:require [clojure.math.numeric-tower :as numeric])
  (:use cexirt.limath)
  (:use cexirt.transform)
  (:use cexirt.ray)
  (:use [clojure.pprint :only [pprint]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defprotocol Transformable
  (transform-object [this T]))

(definterface Bounding
  (^double width [])
  (^double height [])
  (^double depth []))

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



(deftype Intersection [point normal])
    
;; "Ray to shape intersection methods"
(defprotocol RayIntersection
  ( intersect [this r]);; "Distance to intersection")
  )

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

(defn bbox-centre [^BoundingBox3 b]
  (point3 (* (+ (.x0 b) (.x1 b)) 0.5)
             (* (+ (.y0 b) (.y1 b)) 0.5)
             (* (+ (.z0 b) (.z1 b)) 0.5)))
(defn bbox-min [^BoundingBox3 b]
  (point3 (.x0 b) (.y0 b) (.z0 b)))

(defn bbox-max [^BoundingBox3 b]
  (point3 (.x1 b) (.y1 b) (.z1 b)))

(defn bbox-size [^BoundingBox3 b] (vector3 (.width b) (.height b) (.depth b)))

(defn bounding-box3-intersect-ray [^BoundingBox3 B ^Ray r]
  (let [ea (vsub3 (bbox-min B) (.origin r))
        slab-intersect (fn [^long i ^double t-min ^double t-max]
                         (let [f ((.direction r) i)
                               e (ea i)]
                           (if (> (numeric/abs f) eps-small)
                             (let [e1 (+ e (.width B))
                                   t1' (/ e1 f)
                                   t2' (/ e f)
                                   [t1 t2] (if (> t1' t2') [t2' t1'] [t1' t2'])
                                   t-min' (max t-min t1)
                                   t-max' (min t-max t2)]
                       (if-not (or (> t-min' t-max') (< t-max' 0))
                         [t-min' t-max']))
                             (if-not (or (> (- (+ e (.width B))) 0) (< (- e) 0))
                               [t-min t-max]))))]
      
    (if-let [[tmin-x tmax-x] (slab-intersect 0 (- infinity) infinity)]
      (if-let [[tmin-y tmax-y] (slab-intersect 1 tmin-x tmax-x)]
        (if-let [[tmin tmax] (slab-intersect 2 tmin-y tmax-y)]
          (let [t (if (> tmin 0.0) tmin tmax)]
            (if (> (.maxt r) t (.mint r)) t)))))))
  
(extend-type BoundingBox3  
  RayIntersection
  (intersect [this _r]
    (let [^Ray r _r]
      (bounding-box3-intersect-ray this r)))
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

(deftype Primitive [shape transform]
  RayIntersection
  (intersect [this _r]
    (let [^Ray r _r]
      0.0))
  Object
  (toString [this] (str shape " " transform)))
