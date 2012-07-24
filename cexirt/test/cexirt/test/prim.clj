(ns cexirt.test.prim
  (:use [cexirt.prim])
  (:use [cexirt.transform])
  (:use [cexirt.geom])
  (:use [clojure.test]))


(defn four-spheres []
  (for [y (range 0 3 2) x (range 0 3 2)]
    (primitive (translate x y 0) (sphere-shape 1.0))))


(deftest trace-prim-test-miss []
         (is (= (trace (primitive 
                              (identity-transform)
                              (sphere-shape 1.0)) 
                            (ray (point3 1.1 0 2) (vector3 0 0 -1)))
                     nil)))

(deftest trace-prim-test-hit []
         (is (not (= (trace (primitive 
                              (translate 2.0 0.0 0) 
                              (sphere-shape 1.0)) 
                            (ray (point3 1.8 0 2) (vector3 0 0 -1)))
                     nil))))

(deftest trace-prim-list-one []
         (is (not (= nil
              (trace (primitive-list 
                       (translate 0 0 0)
                       [(primitive (translate 0 0 0) (sphere-shape 1.0))])
                     (ray (point3 0 0 3) (vector3 0 0 -1)))))))

(deftest trace-prim-list []
         (is (not (= nil
                     (trace (primitive-list
                              (translate -1 -1 0)
                              (four-spheres))
                            (ray (point3 1 1 3) (vector3 0 0 -1)))))))

(comment 
  (do
    (def p1 (primitive (translate 2 0 0) (sphere-shape 1.0)))
    (def p2 (primitive (translate 6 0 0) (sphere-shape 2.0)))
    (def pl1 (primitive-list (translate 0 0 0) [p1 p2]))
    (def r1 (ray (point3 -4 0 0) (vector3 1 0 0)))))
