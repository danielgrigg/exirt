(ns cexirt.geom
  (:require [clojure.math.numeric-tower :as numeric])
  (:use cexirt.limath)
  (:use cexirt.transform)
  (:use [clojure.pprint :only [pprint]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(import cexirt.transform.Transform)

(defprotocol Transformable
  (transform-object [this T] "Transform the object by T"))

(defprotocol Bounded
  (bounding-box [this] "Compute the AABB"))

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

(defn ray-interval "Confine the ray r to an interval"
  ([^Ray r ^double mint ^double maxt]
     (Ray. (.origin r) (.direction r) mint maxt))
  ([^Ray r ^double maxt]
     (Ray. (.origin r) (.direction r) (.mint r) maxt)))

(defn ^Ray ray
  ([origin direction]
     (Ray. origin direction eps infinity))
  ([origin direction mint maxt]
     (Ray. origin direction mint maxt)))

(deftype Intersection [point normal])
    
;; "Ray to shape intersection methods"
(defprotocol RayIntersection
  ( intersect [this r] "Distance to intersection"))

(deftype BBox [minp maxp]
  Bounding
  (width [this] (- (maxp 0) (minp 0)))
  (height [ this] (- (maxp 1) (minp 1)))
  (depth [this] (- (maxp 2) (minp 2)))

  Bounded
  (bounding-box [this]
    this)
  
  Transformable
  (transform-object [this T]
    (let [p0 (transform-point minp T)
          p1 (transform-point maxp T)]
      (BBox. (vmin4 p0 p1) (vmax4 p0 p1))))
    
  Object
  (toString [this]
    (str minp " " maxp)))

(defn bbox "Construct a BBox"
  ([] 
     (BBox. (point3 0 0 0) (point3 0 0 0)))  
 ([[x0 y0 z0 w0 :as p0] [x1 y1 z1 w1 :as p1]]
     (BBox. (vmin4 p0 p1) (vmax4 p0 p1))))

(defn bbox-centre [^BBox b]
  (vmul4 (vadd4 (.minp b) (.maxp b)) [0.5 0.5 0.5 1.] ))

(defn bbox-size "BBox [width height depth]" [^BBox b] (vector3 (.width b) (.height b) (.depth b)))

(defn bbox-union-point "Union of BBox b and point p" [^BBox b p]
  (BBox. (vmin4 (.minp b) p) (vmax4 (.maxp b) p)))

(defn bbox-union-bbox "Union of bboxes a and b." [^BBox a ^BBox b]
  (bbox-union-point (bbox-union-point a (.minp b))
                    (.maxp b)))
(defn bbox-contains-point "BBox contains point p" [^BBox a p]
  (let [[x0 y0 z0] (.minp a)
        [x1 y1 z1] (.maxp a)]
    (not (or (< (p 0) x0) (< (p 1) y0) (< (p 2) z0)
             (> (p 0) x1) (> (p 1) y1) (> (p 2) z1)))))

(defn intersect-bbox-ray "Intersection distance of r to B." [^BBox B ^Ray r]
  (let [ea (vsub3 (.minp B) (.origin r))
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

(extend-type BBox  
  RayIntersection
  (intersect [this _r]
    (let [^Ray r _r]
      (intersect-bbox-ray this r))))

(deftype Plane [position normal]
  RayIntersection
  (intersect [this _r]
    (let [^Ray r _r
          rdotn (vdot3 (.direction r) normal)]
      (if (> (numeric/abs rdotn) eps-small)
        (let [t (/ (vdot3 (vsub3 position (.origin r)) normal) rdotn)]
          (if (> (.maxt r) t (.mint r)) t)))))

  Bounded
  (bounding-box [this]
    ;; Bounding-box for an infinite plane...
    (bbox))
  Object 
  (toString [this] (str position " " normal)))

(defn plane [position normal] (Plane. position normal))

(defn intersect-osphere-ray [^double radius ^Ray r]
  (let [A (vdot3 (.direction r) (.direction r))
        B (* 2.0 (vdot3 (.direction r) (.origin r)))
        C (- (vdot3 (.origin r) (.origin r)) (* radius radius ))]
    (if-let [t (quadratic A B C)]
      (let [t0 (first t)
            t1 (second t)]
        (if-not (or (> t0 (.maxt r)) (< t1 (.mint r)))
          (if (< t0 (.mint r))
            (if (> (.maxt r) t1) t1)
            t0))))))

(deftype OSphere [^double radius]
  RayIntersection
  (intersect [this r]
    (intersect-osphere-ray radius))
  Object
  (toString [this] (str radius)))

(defn osphere [^double r] (OSphere. r))

;; multiple dispatch would be so elegant..

(defn intersect-bbox-sphere [^BBox b ^double radius [sx sy sz] ]
  (let [[x0 y0 z0] (.minp b)
        [x1 y1 z1] (.maxp b)]
         (not (> (+ (if (< sx x0) (sq (- sx x0)) (if (> sx x1) (sq (- sx x1)) 0))
                    (if (< sy y0) (sq (- sy y0)) (if (> sy y1) (sq (- sy y1)) 0))
                    (if (< sz z0) (sq (- sz z0)) (if (> sz z1) (sq (- sz z1)) 0)))
                (sq radius)))))

(defn intersect-bbox-bbox [^BBox a ^BBox b]
  (let [a0 (.minp a) a1 (.maxp a)
        b0 (.minp b) b1 (.maxp b)]
    (not (or (> (a0 0) (b1 0)) (> (b0 0) (a1 0))
             (> (a0 1) (b1 1)) (> (b0 1) (a1 1))
             (> (a0 2) (b1 2)) (> (b0 2) (a1 2))))))

(defn intersect-triangle-ray [p0 p1 p2 ^Ray r]
  (let [e1 (vsub3 p1 p0)
        e2 (vsub3 p2 p0)
        pvec (cross (.direction r) e2)
        tvec (vsub3 (.origin r) p0)
        qvec (cross tvec e1)
        det (vdot3 e1 pvec)
        u (vdot3 tvec pvec)
        v (vdot3 (.direction r) qvec)]
    (if (and (> det eps)
             (>= u 0.0) (<= u det)
             (>= v 0.0) (<= (+ u v) det))
      (vdiv3s [(vdot3 e2 qvec) u v] det))))

(deftype Triangle [p0 p1 p2]
  RayIntersection
  (intersect [this r]
    (let [^Ray _r r]
      ((intersect-triangle-ray p0 p1 p2 _r)) 0))
  Object
  (toString [this] (str p0 " " p1 " " p2)))

(defn triangle [p0 p1 p2]
  (Triangle. p0 p1 p2))

