(ns cexirt.core
  (:gen-class)
  ;;  (:require [incanter [core :as icc]])
  (:use cexirt.essentials)
  (:use cexirt.geom)
  (:use cexirt.limath)
  (:use cexirt.film)
  (:use cexirt.camera)
  (:require cexirt.forge)
  (:use [clojure.pprint :only [pprint]]))
                
(defn test-screen-coverage [width height]
  (finish-framebuffer
   width height
   (map (fn [[r g]] [r g 0.0 1.0]) 
        (map
         (fn [r] (let [d (.direction r)] (vnormalize4 [(d 0) (d 1) 0.5 0.0])))
         (camera-rays width height (perspective (Math/toRadians 38.) 1.0 1.0 100.))))))

(def ^:dynamic *world* (new-sphere 2.0))
(def ^:dynamic *world-transform* (translate 0 0 -6))
(def ^:const background [0.0 0.0 0.0 1.0])
(def ^:const default-perspective (perspective (Math/toRadians 38.) 1.0 1.0 100.))

(defn trace-world [ray]
  (let [r-world (transform ray (inverse *world-transform*))]
    (if-let [hit (intersect *world* r-world)]
      (ray-at r-world (second hit)))))

(defn shade-diffuse [p]
  (if p
    (let [l (max 0.0 
                 (vdot4 (vnormalize4 (vsub4 p (point3 0 0 0))) 
                        (vector3 1 1 1)))]
      [l l l 1.0])
    background))

(defn sphere-trace [width height fov process-fn]
  (let [P (perspective (Math/toRadians fov) 1.0 1.0 100.)]        
    (map process-fn
         (map trace-world
              (camera-rays width height (perspective (Math/toRadians fov) 1. 1. 100.))))))

(defn new-rgba [rgb a]
  [(rgb 0) (rgb 1) (rgb 2) a])

(defn shade-point [p]
  (if p p background))

(defn shade-normal [p]
  (if p
    (let [rgb (xyz (vadd4s (vmul4s (vnormalize4 (vsub4 p (point3 0 0 0))) 0.5) 0.5))]
      [(Math/pow (rgb 0) 2.22)
       (Math/pow (rgb 1) 2.22)
       (Math/pow (rgb 2) 2.22)
       1.])
    background))

(defn -main [& args]
  (let [[w h fov r] (map read-string (take 4 args))]
    (println "Running cexirt with "
             (/ (.maxMemory (java.lang.Runtime/getRuntime)) 1024.0 1024.0) " MB memory")
    (println "w " w " h " h " fov " fov " r " r)
    ;;    (test-screen-coverage)
    
    (let [f (cexirt.forge/make-sin-clamped-f)]
      (finish-framebuffer w h (cexirt.forge/graph2 w h f)))
    
    (comment (binding [*world* (new-sphere r)]
      (finish-framebuffer
       w h
       (map dither
            (sphere-trace w h fov shade-diffuse)))))))

