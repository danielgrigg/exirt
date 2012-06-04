(ns cexirt.camera
  (:use cexirt.essentials)
  (:use cexirt.limath)
  (:use cexirt.geom)
  (:use cexirt.film))

(defn new-camera-ray [x y camera-to-screen]
  "Create a ray in camera space from a pixel-coordinate using the camera-to-screen transform."
  (new-ray (point3 0 0 0)
       (vsub4 (transform-point (point3 x y -1) (inverse camera-to-screen))
              (point3 0 0 0))))

(defn camera-rays [width height proj]
  (let [S (screen-transform width height)
        SP (compose S proj)]
    (for [y (range 0 height)
          x (range 0 width)]
      (new-camera-ray x y SP))))
