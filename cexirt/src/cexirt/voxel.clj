(ns cexirt.voxel
  (:require [clojure.math.numeric-tower :as numeric])
  (:use cexirt.limath)
  (:use cexirt.transform)
  (:use cexirt.geom)
  (:use [clojure.pprint :only [pprint]]))


;;(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(import cexirt.geom.BoundingBox3)

(def ^:dynamic *grid-bounds* (bounding-box3 [2 2 2] [7 9 13]))
(def ^:dynamic *test-ray* (ray (point3 0.5 7 5) (vector3 -2.0 -0.85 0.4)))

;(def ^:dynamic *grid-bounds* (bounding-box3 [0 0 0] [7 5 2]))
;(def ^:dynamic *test-ray* (ray (point3 1.7 0.5 1.0) (vector3 2.0 0.85 0.0)))

(defn sign-step [x]
  (if (pos? x) 1 -1))

(defn voxel-size [^BoundingBox3 b d]
  (vdiv3 (bbox-size b) d))

(defn voxel-index-from-point [grid-bounds divisions p]
  (vec (map int (vdiv3 (vsub3 p (bbox-min grid-bounds))
                       (voxel-size grid-bounds divisions)))))

(defn voxel-index-to-point [grid-bounds divisions v]
  (vadd3 (bbox-min grid-bounds)
         (vmul3 v (voxel-size grid-bounds divisions))))

(defn- zero-min [^double x]
  (if (> (Math/abs x) eps-small) x eps-small))

;; work in progress.  The goal is a voxel-seq function.  More complex voxel queries can be built on-top.
;; Should probably wrap everything in a VoxelGrid protocol or some such. For now, we just use
;; some loosely coupled functions - which I kinda like actually, apart from the param redundancy..
(defn voxel-walk2 [grid-bounds ndiv r]
  (let [o (.origin r)
        d' (.direction r)
        d (vec (map zero-min d'))
        [sx sy sz :as s] (vec (map sign-step d'))
        v0 (voxel-index-from-point grid-bounds ndiv o)
        v1 (vadd3 v0 (vmax3 s [0 0 0]))
        tmax (vdiv3 (vsub3 (voxel-index-to-point grid-bounds
                                                 ndiv
                                                 v1) o) d)
        [tdx tdy tdz :as td] (vabs3 (vdiv3 (voxel-size grid-bounds ndiv) d))
        
         step-fx (fn step-f [[x y z] [tmax-x tmax-y]] 
                  (lazy-seq (cons [x y z] (if (< tmax-x tmax-y)
                                          (step-f [(+ x sx) y z]
                                                  [(+ tmax-x tdx) tmax-y]
                                                  )
                                          (step-f [x (+ y sy) z]
                                                  [tmax-x
                                                   (+ tmax-y tdy)
                                                   ])))))]
;    [s v0 v1 tmax td]))
    (step-fx v0 tmax)))

; could certainly be better optimised...but let's stick with 'simplicity' for now.
(defn voxel-seq3 [grid-bounds [nx ny nz :as ndiv] r]
  (when-let [enter-t (intersect grid-bounds r)]
    (let [o (ray-at r enter-t)
          d' (.direction r)
          d (vec (map zero-min d'))
          [sx sy sz :as s] (vec (map sign-step d'))
          v0 (vmin3 (voxel-index-from-point grid-bounds ndiv o)
                    (vsub3 ndiv [1 1 1]))
          v1 (vadd3 v0 (vmax3 s [0 0 0]))
          tmax (vabs3 (vdiv3 (vsub3 (voxel-index-to-point grid-bounds
                                                   ndiv
                                                   v1) o) d))
          [tdx tdy tdz :as td] (vabs3 (vdiv3 (voxel-size grid-bounds ndiv) d))
          
          step-fx (fn step [[x y z] [tmx tmy tmz]]
                    (if (and (< x nx) (>= x 0)
                             (< y ny) (>= y 0)
                             (< z nz) (>= z 0)) 
                      (cons [x y z]
                            (lazy-seq
                             (if (< tmx tmy)
                               (if (< tmx tmz)                                 
                                 (step [(+ x sx) y z]
                                       [(+ tmx tdx) tmy tmz])
                                 (step [x y (+ z sz)]
                                       [tmx tmy (+ tmz tdz)]))
                             (if (< tmy tmz)
                               (step [x (+ y sy) z]
                                     [tmx (+ tmy tdy) tmz])
                               (step [x y (+ z sz)]
                                     [tmx tmy (+ tmz tdz)])))))))]
      (step-fx v0 tmax))))
  
                                   