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
  (:use cexirt.prim)
  (:require cexirt.forge)
  (:use [clojure.pprint :only [pprint]]))

(import cexirt.geom.Ray)
(import cexirt.sampling.Sample)

;(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def world1 (atom
             (primitive (translate -1 -1 0)
                        (primitive-list
                         [(primitive (compose (rotate-x (Math/toRadians 130))
                                              (scale 1 1 2.5))
                                     (sphere-shape 2.0))
                          (primitive (compose (translate 0 -2 -1)
                                              (rotate-x (Math/toRadians 90))
                                              (scale 2.5 1 2.5))
                                     (sphere-shape 2.0))]))))
  
(def world2 (atom
             (primitive (translate 0 0 0)
                        (primitive-list
                         [              
                          (primitive (translate 0 0 -2) (sphere-shape 2.0))
                          (primitive (compose (translate 0 0 -0.8)
                                              (scale (rand 2.0) (rand 2.0) (rand 2.0)))
                                     (sphere-shape 1.0))
                          ]))))
(def world3 (atom (primitive
                   (compose 
                    (rotate-x (Math/toRadians 130))
                    (scale 1 1 2.5))                    
                   (sphere-shape 2.0))))

(defn scale-rand []
  (scale (+ 0.4 (rand 1.2)) (+ 0.4 (rand 1.2)) 1.0))

(def world4 (atom
             (primitive 
              (translate -4.9 -3 -3)
              (primitive-list
               (for [z (range 0 7 3)
                     y (range 0 7 3)
                     x (range 0 10 3.3)]
                 (primitive (compose (translate x y z)
                                     (rotate-axis (rand 3.14)
                                                  (vnormalize4 (vector3 (rand) (rand) (rand))))
                                     (scale-rand))
                            (sphere-shape 1.0)))))))

(defn complex-world [n]
  (let [t (/ (* 10 (dec n)) -2)
        f (fn [v] (+ v (* (rand) 6.0)))]
    (primitive (translate t t t)
               (primitive-grid2
                (for [z (range 0 (* 10 n) 10)
                        y (range 0 (* 10 n) 10)
                        x (range 0 (* 10 n) 10)]
                  (primitive (translate (f x) (f y) (f z))
                             (sphere-shape (+ 1.0 (rand)))))))))


(def world5 (atom (complex-world 100)))

(def world world5)
(def world-transform (atom (compose (translate 0.0 0.0 0)
                                    (rotate-axis (Math/toRadians 0.0)
                                                 (vector3 1 1 0)))))

(defn voxel-evaluator [screen-w screen-h proj-t]
  (let [S (screen-transform screen-w screen-h)
        SP (compose S proj-t)]
    (fn [^Sample sample]
      (let [^Ray r-camera (camera-ray (.x-film sample) (.y-film sample) SP)
            ^Ray r-world (transform-object r-camera (inverse @world-transform))
            grid-bounds (bbox (point3 -1 -1 -1) (point3 1 1 1))
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
            L (radiance r-world world)] 
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
           (cexirt.forge/prender film filter sampler f 4 16)
           frame-path))
        nil)))

