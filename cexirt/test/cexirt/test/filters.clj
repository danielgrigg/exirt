(ns cexirt.test.filter
  (:use [cexirt.filters])
  (:use [clojure.test])
  (:use [cexirt.limath]))

(comment (do (view-scatter2
              (let [f (gaussian-filter 4.0 0.1)]
                (for [x (range -4.0 4.01 0.02)] [x (.evaluate f x 0.0)])))
             (view-scatter2
              (let [f (mitchell-filter 5.0 (/ 3.0) (/ 3.0))]
                (for [x (range -5.0 5.01 0.01)] [x (.evaluate f x 0.0)])))))
