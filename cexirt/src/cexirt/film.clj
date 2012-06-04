(ns cexirt.film
  (:use cexirt.essentials)
  (:use cexirt.limath)
  (:use cexirt.geom))

(defn native-framebuffer [fb]
  (float-array (apply concat fb)))

(defn finish-framebuffer [width height fb]
    (jna-call :exr_basic "write_rgba" Integer width height
              (native-framebuffer fb)))

;; NDC to screen coordinates
(defn screen-transform [w h]
  (compose (translate 0 h 0) (scale w (- h) 1) (scale 0.5 0.5 1.0) (translate 1 1 0)))

(defn dither [c]
  (let [dk 0.0001
        n4 (vmul4s (vec (concat (rand-gauss2) (rand-gauss2))) dk)]
    (vadd4 c n4)))

