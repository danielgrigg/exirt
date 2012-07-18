(ns cexirt.core
  (:gen-class)
  ;;  (:require [incanter [core :as icc]])
  (:use cexirt.essentials)
  (:use cexirt.geom)
  (:use cexirt.limath)
  (:use cexirt.transform)
  (:use cexirt.sampling)
  (:use cexirt.filters)
  (:use cexirt.film)
  (:use cexirt.camera)
  (:use cexirt.voxel)
  (:require cexirt.forge)
  (:use [clojure.pprint :only [pprint]]))

(import cexirt.geom.Ray)
(import cexirt.sampling.Sample)

;(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def world (atom (bounding-box3 (point3 -1 -1 -1) (point3 1 1 1))))
;(def world-transform (atom (translate 0 0 -6)))
(def world-transform (atom (compose (translate 0 0 -5)
                                    (rotate-axis 1.0 (vector3 1 1 0)))))


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

(defn shade-distance [t]
  (vec (repeat 3 (/ t 10.0))))

(defn shade-checker [p]
  (let [w 1.5
        fx (if (>= (mod (p 0) (* w 2.0)) w) 1 0)
        fz (if (>= (mod (p 2) (* w 2.0)) w) 1 0)]
    (if (= 1 (bit-xor fx fz))
      [1.0 1.0 1.0]
      [0.0 0.0 0.0])))

(defn voxel-evaluator [screen-w screen-h proj-t]
  (let [S (screen-transform screen-w screen-h)
        SP (compose S proj-t)]
    (fn [^Sample sample]
      (let [^Ray r-camera (camera-ray (.x-film sample) (.y-film sample) SP)
            ^Ray r-world (transform-object r-camera (inverse @world-transform))
            grid-bounds (bounding-box3 (point3 -1 -1 -1) (point3 1 1 1))
            L (if-let [hit (intersect grid-bounds r-world)]
                (let [n (count (voxel-seq3 grid-bounds [5 5 5] r-world))
                      m (/ n 13.0)]
                  [m m m ])
                [0. 0. 0. ])]
        (sample-radiance sample L)))))

        
(defn trace-evaluator [screen-w screen-h proj-t]
  (let [S (screen-transform screen-w screen-h)
        SP (compose S proj-t)]
    (fn [^Sample sample]
      (let [^Ray r-camera (camera-ray (.x-film sample) (.y-film sample) SP)
            ^Ray r-world (transform-object r-camera (inverse @world-transform))
            L (if-let [hit (intersect @world r-world)]
                (shade-normal (ray-at r-world hit))
                                        ;[1.0 1.0 1.0]
                ;(shade-checker (ray-at r-world hit))
                [0. 0. 0. ])]
        (sample-radiance sample L)))))

(defn plot-evaluator [width height func]
  (let [to-nc (fn [^double x ^double y ^double w ^double h]
                [(- (* (/ x w) 2.0) 1.0) (- (* (/ y h) 2.0) 1.0)])]
    (fn [^Sample sample]
      (let [xy (to-nc (.x-film sample) (.y-film sample) width height)]
        (sample-radiance sample (func xy))))))

(defn -main [& args]
  (let [[w h fov n-samples frame-n] (map read-string (take 5 args))
        out-path (nth args 5 "out")
        P (perspective (Math/toRadians fov) (double (/ w h)) 1.0 1000.)
        f (trace-evaluator w h P)        
        g (plot-evaluator w h cexirt.forge/sin-theta-f)
        v-f (voxel-evaluator w h P)
        sampler (cexirt.sampling/sampler-stratify2 n-samples)
        film (film-new (film-rect-width-height 0 0 w h))
        ;;          filter (table-filter (gaussian-filter) 16)]
        filter (gaussian-filter)          
        ]
    (do
      (println "Running cexirt with "
               (/ (.maxMemory (java.lang.Runtime/getRuntime)) 1024.0 1024.0) " MB memory")
      (println "w " w " h " h " fov " (max 1. fov) " path " out-path)

      (let [frame-path (str out-path "_" frame-n ".exr")
            t (/ frame-n 24.0)
            frame-transform (fn [old u] (compose (translate 0 0 -4)
                                                 (rotate-axis (* u 6.28 0.5 0.1) (vector3 0 0 1))))]
          (println "rendering " frame-path)
  ;;        (swap! world-transform frame-transform t)
          (framebuffer-finish
           (cexirt.forge/prender film filter sampler v-f 4 16)
           frame-path))
        nil)))

