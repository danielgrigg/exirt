(ns cexirt.shading
  (:use cexirt.essentials)
  (:use cexirt.limath)
  (:use cexirt.transform))

;; This is all pretty ad-hoc and subject to radical change..
(defn shade-diffuse [[t [position normal]]]
  (let [l (max 0.0 
               (vdot4 (vnormalize4 normal)
                      (vnormalize4 (vector3 0.1 0.4 1))))]
    [l l l]))

(defn shade-normal [[t [P N]]]
  (let [rgb (xyz (vadd4s (vmul4s (vnormalize4 N) 0.5) 0.5))]
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
