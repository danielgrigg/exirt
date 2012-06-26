(ns cexirt.core
  (:gen-class)
  ;;  (:require [incanter [core :as icc]])
  (:use cexirt.essentials)
  (:use cexirt.geom)
  (:use cexirt.limath)
  (:use cexirt.sampling)
  (:use cexirt.filters)
  (:use cexirt.film)
  (:use cexirt.camera)
  (:require cexirt.forge)
  (:use [clojure.pprint :only [pprint]]))

(import cexirt.geom.Ray)
(import cexirt.sampling.Sample)

;(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:dynamic *world* (new-sphere 2.0))
(def ^:dynamic *world-transform* (translate 0 0 -6))
(def ^:const default-perspective (perspective (Math/toRadians 38.) 1.0 1.0 100.))

(defn shade-diffuse [p]
  (let [l (max 0.0 
               (vdot4 (vnormalize4 (vsub4 p (point3 0 0 0))) 
                      (vector3 1 1 1)))]
    [l l l]))

(defn shade-normal [p]
  (let [rgb (xyz (vadd4s (vmul4s (vnormalize4 (vsub4 p (point3 0 0 0))) 0.5) 0.5))]
    [(Math/pow (rgb 0) 2.22)
     (Math/pow (rgb 1) 2.22)
     (Math/pow (rgb 2) 2.22)]))

(defn trace-evaluator [screen-w screen-h proj-t world-t world]
  (let [S (screen-transform screen-w screen-h)
        SP (compose S proj-t)]
    (fn [^Sample sample]
      (let [^Ray r-camera (camera-ray (.x-film sample) (.y-film sample) SP)
            ^Ray r-world (transform-object r-camera (inverse world-t))
            L (if-let [hit (intersect world r-world)]
                (shade-normal (ray-at r-world (second hit)))
                [0. 0. 0. ])]
        (sample-radiance sample L)))))

(defn plot-evaluator [width height func]
  (let [to-nc (fn [^double x ^double y ^double w ^double h]
                [(- (* (/ x w) 2.0) 1.0) (- (* (/ y h) 2.0) 1.0)])]
    (fn [^Sample sample]
      (let [xy (to-nc (.x-film sample) (.y-film sample) width height)]
        (sample-radiance sample (func xy))))))

(defn -main [& args]
  (let [[w h fov n-samples] (map read-string (take 4 args))
        out-path (nth args 4 "out.exr")
        f (trace-evaluator w h
                           (perspective (Math/toRadians fov) 1.0 1.0 100.)
                           *world-transform*
                           *world*)
        g (plot-evaluator w h cexirt.forge/sin-theta-f)
        sampler (cexirt.sampling/sampler-stratify2 n-samples)
        film (film-new (film-rect-width-height 0 0 w h))
        ;;          filter (table-filter (gaussian-filter) 16)]
        filter (gaussian-filter)          
        ]
    (do
      (println "Running cexirt with "
               (/ (.maxMemory (java.lang.Runtime/getRuntime)) 1024.0 1024.0) " MB memory")
      (println "w " w " h " h " fov " (max 1. fov) " path " out-path)
      (framebuffer-finish
       (cexirt.forge/prender film filter sampler g 4 16)
       out-path)))
  nil)

