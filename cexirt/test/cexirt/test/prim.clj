(ns cexirt.test.prim
  (:use [cexirt.transform])
  (:use [cexirt.geom])
  (:use [cexirt.prim])
  (:use [clojure.test]))


(defn four-spheres []
  (vec (for [y (range 0 3 2) x (range 0 3 2)]
    (primitive (translate x y 0) (sphere-shape 1.0)))))


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
                       [(primitive (translate 0 0 0) (sphere-shape 1.0))])
                     (ray (point3 0 0 3) (vector3 0 0 -1)))))))

(deftest trace-prim-list []
         (is (not (= nil
                     (trace (primitive (translate -1 -1 0)
                                       (primitive-list (four-spheres)))
                            (ray (point3 1 1 3) (vector3 0 0 -1)))))))

