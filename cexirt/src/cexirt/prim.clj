(ns cexirt.prim
  (:use cexirt.limath)
  (:use [cexirt.geom :exclude [Sphere Triangle]])
  (:use cexirt.transform)
  (:use cexirt.shading)
  (:use cexirt.voxel)
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
      (if-let [[t [p n]] (trace shape (transform-object r (inverse transform)))]
        [t [(transform-point p transform) (transform-normal n transform)]])))

  Bounded
  (bounding-box [this]
;    (bounding-box shape))
    (transform-object (bounding-box shape) transform))
  
  Object
  (toString [this] (str {:bbox (bounding-box this)
                         :transform transform
                         :shape shape})))

(defn primitive [^Transform transform shape]
  (Primitive. transform shape))

(defn trace-many [ps _r]
  (let [^Ray r _r
        [tn i :as tni] (reduce (fn [[t0 _ :as s1] prim]
                                 (do
                                   (println "s1 " s1 " prim " prim)
                                   (if-let [s2 (trace prim (ray-interval r t0))]
                                     s2
                                     s1)))
                               [(.maxt r) nil]
                               ps)]
    (if i tni)))

(deftype PrimitiveList [^BBox bounds primitives]
  Traceable
  (trace [this _r]
    (let [^Ray r _r
          [tn i :as tni] (reduce (fn [[t0 _ :as s1] prim]
                                   (if-let [s2 (trace prim (ray-interval r t0))]
                                     s2
                                     s1))
                                 [(.maxt r) nil]
                                 primitives)]
      (if i tni)))

  Bounded
  (bounding-box [this]
    bounds)

  Object
  (toString [this] (str {:bbox bounds                         
                         :nprimitives (count primitives)})))

(defn- primitive-list-bounds [ps]
  (reduce bbox-union-bbox (map bounding-box ps)))

(defn primitive-list "Construct a PrimitiveList from a primitive collection ps."
  [ps]
  (PrimitiveList. (primitive-list-bounds ps) ps))

(defmacro indexer3 [width height depth]
  `(fn [^long x# ^long y# ^long z#]
     (+ (* z# (* ~width ~height))
        (* y# ~width)
        x#)))

;(defn voxel-at [^PrimitiveGrid grid ^long x ^long y ^long z]
;  ((.voxels grid) (+ (* z (* ((.divisions grid) 0) ((.divisions grid) 1)))
;                     (* y ((.divisions grid) 0))
;                     x)))

(deftype PrimitiveGrid [^BBox bounds divisions voxels]
  Traceable
  (trace [this r']
    (let [^Ray r r'
          idx-f (indexer3 (divisions 0) (divisions 1) (divisions 2))
          voxel-at (fn [[x y z]]
                     (voxels (+ (* z (* (divisions 0) (divisions 1)))
                                (* y (divisions 0))
                                x)))
          voxel-trace (fn [v]
                        (let [[t i  :as hit]
                              (reduce
                               (fn [[t0 _ :as s1] prim]
                                 (if-let [s2 (trace prim (ray-interval r t0))]
                                   s2
                                   s1))
                               [(.maxt r) nil]
                               v)]
                          (if i hit)))
          voxels (map voxel-at (voxel-seq3 bounds divisions r))]
      (first (keep identity (map voxel-trace voxels)))))

  Bounded
  (bounding-box [this]
    bounds)

  Object
  (toString [this] (str {:bounds bounds :divisions divisions :voxels (count voxels)})))
        
(defn array3-subspace "Sequence the elements bounded by (v0, v1)."
  [[v0 v1]]
  (for [z (range (v0 2) (inc (v1 2)))
        y (range (v0 1) (inc (v1 1)))
        x (range (v0 0) (inc (v1 0)))]
    [x y z]))

(defn voxel-subspace-from-bbox [^BBox grid-bounds ndiv3 ^BBox b]
  [(voxel-index-from-point grid-bounds ndiv3 (.minp b))
   (voxel-index-from-point grid-bounds ndiv3 (.maxp b))])

(defn primitive-grid-old [ps]
  (let [bounds' (primitive-list-bounds ps)
        bounds (bbox (vsub4 (.minp bounds') (vector3 1 1 1))
                     (vadd4 (.maxp bounds') (vector3 1 1 1)))
        ndiv (max 2 (Math/floor (/ (Math/pow (count ps) (/ 3.0)) 3.0)))
        ndiv3 [ndiv ndiv ndiv]

        index-f (indexer3 ndiv ndiv ndiv)
        add-primitive (fn [grid [x y z] p]
                        (let [idx (index-f x y z)]
                          (assoc grid idx (conj (grid idx) p))))]
;    [bounds ndiv3 (voxel-index-from-bbox (bounding-box (first ps))) add-primitive]))
   
    (loop [G (vec (repeat (* ndiv ndiv ndiv) []))
           [q :as qs] ps]
      (if (nil? q)
        G
        (recur (loop [G' G
                      [v :as vs] (array3-subspace (voxel-subspace-from-bbox
                                                   bounds
                                                   ndiv3
                                                   (bounding-box q)))]
                 (if (nil? v)
                   G'
                   (recur (add-primitive G' v q) (rest vs))))
               (rest qs))))))

(defn grid-dimensions "Heuristical grid size for n primitives" [n]
  ;; Assume uniform divisions per axis for now
  (let [n (max 3 (int (Math/floor (/ (Math/pow n (/ 3.0)) 2.0))))]
    [n n n]))

(defn primitive-grid2 [ps]
  (let [bounds' (primitive-list-bounds ps)
        bounds (bbox (vsub4 (.minp bounds') (vector3 1 1 1))
                     (vadd4 (.maxp bounds') (vector3 1 1 1)))
        ndiv3 (grid-dimensions (count ps))
        index-f (indexer3 (ndiv3 0) (ndiv3 1) (ndiv3 2))

        ;; an interesting function, we could extract it out to 'simplify' the components of grid creation,
        ;; but that seems like refactoring for the sake of it..
        add-prim (fn [grid p]
                   ;; add the primitive to every voxel it overlaps
                   (reduce (fn [grid-out [x y z]]
                             (let [idx (index-f x y z)]
                               (assoc grid-out idx (conj (grid-out idx) p))))
                           grid
                           (array3-subspace (voxel-subspace-from-bbox
                                               bounds
                                               ndiv3
                                               (bounding-box p)))))
        voxels (reduce add-prim
                       (vec (repeat (apply * ndiv3) []))
                       ps)]
    (PrimitiveGrid. bounds
                    ndiv3
                    (vec voxels))))
;                    (vec (map primitive-list voxels)))))

                           
          
(deftype SphereShape [^double radius]
  Traceable
  (trace [this _r]
    (let [^Ray r _r]
      (if-let [t (intersect-osphere-ray radius r)]
        (let [p (ray-at r t)
              n (vsub4 p (point3 0 0 0))]
          [t [p n]]))))
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
    (bbox-union-point (bbox p0 p1) p2))
  Object
  (toString [this] (str p0 " " p1 " " p2)))

(defn triangle-shape [p0 p1 p2] (TriangleShape. p0 p1 02))
    
(defn test-triangles [z0 z1 zs]
  (vec (for [z (range z0 z1 zs)]
         (triangle (point3 -1 -1 z) (point3 1 -1 z) (point3 0 1 z)))))

(defn test-spheres [n]
  (vec (for [z (range 0 (* 10 n) 10)
             y (range 0 (* 10 n) 10)
             x (range 0 (* 10 n) 10)]
         (primitive (translate x y z) (sphere-shape 2.0)))))

(defn spheres2 [n]
  (filter (fn [x] (> (rand) 0.5)) (test-spheres n)))