(ns cexirt.voxel
  (:require [clojure.math.numeric-tower :as math])
  (:use cexirt.limath)
  (:use cexirt.transform)
  (:use cexirt.geom)
  (:use [clojure.pprint :only [pprint]]))

;;(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:dynamic *grid-bounds* (bounding-box3 [0 0 0] [7 5 0]))
(def ^:dynamic *test-ray* (ray (point3 1.7 0.5 0) (vector3 2.0 0.85 0)))

(defn sign-step [x]
  (if (pos? x) 1 -1))

(defn voxel-for-point [grid-bounds [x y z]]
  [(Math/floor x) (Math/floor y) (Math/floor z)])
   

(defn voxel-step [ [tdelta-x tdelta-y] [step-x step-y] [x y z] [tmax-x tmax-y]]
  (lazy-seq (cons [x y] (if (< tmax-x tmax-y)
                          (voxel-step [(+ x step-x) y z]
                                      [(+ tmax-x tdelta-x) tmax-y]
                                      [tdelta-x tdelta-y]
                                      [step-x step-y])
                          (voxel-step [x (+ y step-y)]
                                      [tmax-x (+ tmax-y tdelta-y)]
                                      [tdelta-x tdelta-y]
                                      [step-x step-y])))))
(defn voxel-trace2-2 [grid-bounds r]
  (let [o (.origin r)
        d (.direction r)
        zd (vec (map zero-min d))
        tmax (vdiv3 (vsub3 (vceil3 o) o) zd)
        [step-x step-y] (vec (map sign-step d))
        [tdelta-x tdelta-y] (vdiv3 [1.0 1.0 1.0] zd)
        step-fx (fn step-f [[x y z] [tmax-x tmax-y]] 
                  (lazy-seq (cons [x y] (if (< tmax-x tmax-y)
                                          (step-f [(+ x step-x) y z]
                                                      [(+ tmax-x tdelta-x) tmax-y])
                                          (step-f [x (+ y step-y)]
                                                      [tmax-x (+ tmax-y tdelta-y)])))))]
    (step-fx (voxel-for-point grid-bounds (.origin r)) tmax)))
        
     

(defn zero-min- [^double x]
  (if (> (Math/abs x) eps-small) x eps-small))

; ox + t*rx = ceil(ox)
(defn voxel-trace2 [grid-bounds r]
  (let [o (.origin r)
        d (.direction r)
        zd (vec (map zero-min d))
        tmax (vdiv3 (vsub3 (vceil3 o) o) zd)
        tdelta (vdiv3 [1.0 1.0 1.0] zd)
        voxel-f (partial voxel-step tdelta (vec (map sign-step d)))]

;;    (voxel-step (voxel-for-point grid-bounds (.origin r))
    ;;                tmax)))
      (lazy-seq (cons [x y] (if (< tmax-x tmax-y)
                          (voxel-step [(+ x step-x) y z]
                                      [(+ tmax-x tdelta-x) tmax-y]
                                      [tdelta-x tdelta-y]
                                      [step-x step-y])
                          (voxel-step [x (+ y step-y)]
                                      [tmax-x (+ tmax-y tdelta-y)]
                                      [tdelta-x tdelta-y]
                                      [step-x step-y])))))

                
            