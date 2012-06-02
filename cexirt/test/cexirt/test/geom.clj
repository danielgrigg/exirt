(ns cexirt.test.geom
  (:use [cexirt.geom])
  (:use [clojure.test])
  (:use [cexirt.limath]))


;; (require '(incanter core stats charts))
;; (def Z (for [phi (range 0. (* 2. pi) 0.02)] (transform-point (point3 3 3 0) (rotate-axis phi (vector3 1 1 1)))))
;; (incanter.core/view (incanter.charts/scatter-plot (map first Z) (map second Z)))
;;
(deftest test-rotate-x
         (is (vequal4 (transform-point (point3 1 1 0) (rotate-x (/ pi 4.)))
                      (point3 1.0 0.707106 0.707106))))

(deftest test-rotate-y
         (is (vequal4 (transform-point (point3 0 1 1) (rotate-y (/ pi -4.)))
                      (point3 -0.707106 1.0 0.707106))))

(deftest test-rotate-z
         (is (vequal4 (transform-point (point3 1 -1 1) (rotate-z (/ pi -4.)))
                      (point3 0 -1.414213 1.0))))

(deftest test-rotate-axis
         (is (equal-transform 
               (rotate-axis (Math/toRadians 57.34) (vector3 -1 1.4 0.4))
               (matrix-transform
                 [[0.687199 -0.397216 0.608256 0.0]
                  [-0.015916 0.828845 0.559251 0.0]
                  [-0.726293 -0.393998 0.563260 0.0]
                  [0.0 0.0 0.0 1.0]]))))

(deftest test-perspective
         (is (equal-transform
               (perspective  (Math/toRadians 45.0) 1.0 1.0 100.0)
               (matrix-transform
                 [[2.414213 0.0 0.0 0.0]
                  [0.0 2.414213 0.0 0.0]
                  [0.0 0.0 -1.020202 -2.020202]
                  [0.0 0.0 -1.0 0.0]]))))


(deftest test-compose
         (is (equal-transform 
               (compose 
                 (matrix-transform 
                   [[3.89961,   0.83992,   0.81679,   2.34410],
                    [1.48461,   2.69779,   3.61157,   4.35561],
                    [4.23065,   3.54298,   3.73502,   2.27268],
                    [0.24958,   1.95815,   0.14178,   4.77063]])
                 (matrix-transform 
                   [[0.72062, 2.24390, 1.52629, 0.77430],
                    [2.74601, 0.20943, 0.92333, 1.55114],
                    [2.08074, 2.57786, 2.63903, 1.15078],
                    [1.65702, 1.11806, 2.80008, 2.87643]]))
               (matrix-transform 
                 [[10.70031 13.65265 15.44665 12.00488]
                  [23.21006 18.07626 26.48399 22.01891]
                  [24.31523 22.40451 25.94905 19.60684]
                  [13.75698 6.66946 15.92125 17.11615]]))))


