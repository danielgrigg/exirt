(ns cexirt.camera
  (:use cexirt.essentials)
  (:use cexirt.limath)
  (:use cexirt.geom)
  (:use cexirt.film))

(defn camera-ray 
  "Create a ray in camera space from a pixel-coordinate using the camera-to-screen transform."
  [x y camera-to-screen]
  (new-ray (point3 0 0 0)
       (vsub4 (transform-point (point3 x y -1) (inverse camera-to-screen))
              (point3 0 0 0))))
