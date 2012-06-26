(ns cexirt.forge
  (:use cexirt.essentials)
  (:use cexirt.geom)
  (:use cexirt.limath)
  (:use cexirt.film)
  (:use cexirt.filters)
  (:use cexirt.sampling)
  (:use cexirt.camera)
  (:use [clojure.pprint :only [pprint]])
  (:require [incanter core charts stats]))

(import cexirt.filters.Filter)
(import cexirt.film.FilmRect)
(import cexirt.film.Film)
(import cexirt.sampling.Sample)
(import '(java.util.concurrent Executors))

;(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn view-scatter2 [xy]
  (incanter.core/view (incanter.charts/scatter-plot (map first xy) (map second xy))))

(defn sin-theta-f [xy]
  (let [x (xy 0)
        y (xy 1)
        r (max 0.001 (Math/sqrt (+ (* x x) (* y y))))
        t (Math/asin (/ y r))
        theta (mod (+ t (* 2.0 pi)) (* 2.0 pi))
        v (* (Math/sin (* 8.0 r)) (Math/sin (* 180.0 theta)))
        ve (+ (* 0.5 v) 0.5)
        v2 (if (< v 0.5) 0.0 1.0)]
    [v2 v2 v2 1.0]))
    
(defn pixel-samples
  "generate samples for pixel pxl using sampler-f"
  [sampler-f pxl]
  (map (partial vadd2 pxl) (sampler-f)))

(defn coordinates2 [^long w ^long h]
  "Generate integer coordinates over bounds"
  (for [y (range h) x (range w)] [x y]))
 
(defn block-size [n nblocks]
  (if (zero? (mod n nblocks))
    (quot n nblocks)
    (inc (quot n nblocks))))

(defn prender [^Film film ^Filter filter sampler-f evaluator-f nthreads blocks-per-thread]
  (let [^FilmRect bordered (film-bounds-with-filter
                             (.bounds film)
                             (.width filter))
        samples (partial pixel-samples sampler-f)
        pool (Executors/newFixedThreadPool nthreads)
        npixels (* (film-width bordered) (film-height bordered))
        nblocks (* nthreads blocks-per-thread)
        npixels-task (block-size npixels nblocks)
        tasks (map (fn [ps]
                     (fn []
                       (doseq [xy (mapcat samples ps)]
                         (film-add-sample film
                                          filter
                                          (evaluator-f (sample-new xy))))))
                   (partition-all npixels-task
                                  (coordinates2 (film-width bordered)
                                                (film-height bordered))))]
        (doseq [future (.invokeAll pool tasks)]
          (.get future))
        (.shutdown pool)
        film))

