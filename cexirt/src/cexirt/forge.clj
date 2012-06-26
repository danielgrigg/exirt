(ns cexirt.forge
  (:use cexirt.essentials)
  (:use cexirt.geom)
  (:use cexirt.limath)
  (:use cexirt.film)
  (:use cexirt.filters)
  (:use cexirt.sampling)
  (:use [clojure.pprint :only [pprint]])
  (:require [incanter core charts stats]))

(import cexirt.filters.Filter)
(import cexirt.film.FilmRect)
(import cexirt.film.Film)
(import cexirt.sampling.Sample)
(import '(java.util.concurrent Executors))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn view-scatter2 [xy]
  (incanter.core/view (incanter.charts/scatter-plot (map first xy) (map second xy))))

               
(defn rands [^double s]
  (* s (rand)))

(defn make-sin-clamped-f []
  (let [kr [(rands 10) (rands 10) (rands 10)]
        kp [(rands 3) (rands 3) (rands 3)]
        kt [(rands 20) (rands 20) (rands 20)]
        ks [(rands 5) (rands 5) (rands 5)]
        k-scale (- 1.0 (* 0.5 (rand)))
        k-bias (* 0.5 (rand))]
    
    (do
      (println "kr " kr " kp " kp " kt " kt " ks " ks)
      (fn [xy]
        (let [x (xy 0)
              y (xy 1)
              r (max 0.001 (Math/sqrt (+ (* x x) (* y y))))
              t (Math/asin (/ y r))
              theta (mod (+ t 6.2831853) 6.2831853)]
          (vadd4s
           (vmul4s
            [(Math/sin (* (ks 0) (* (Math/sin (* (kr 0) r)) (Math/sin (+ (kp 0) (* (kt 0) theta))))))
             (Math/sin (* (ks 1) (* (Math/sin (* (kr 1) r)) (Math/sin (+ (kp 1) (* (kt 1) theta))))))
             (Math/sin (* (ks 2) (* (Math/sin (* (kr 2) r)) (Math/sin (+ (kp 2) (* (kt 2) theta))))))
             1.0]
            k-scale)
           k-bias))))))

(defn sin-f [xy]
  (let [x (xy 0)
        y (xy 1)
        r (max 0.001 (Math/sqrt (+ (* x x) (* y y))))
        t (Math/asin (/ y r))
        theta (mod (+ t (* 2.0 pi)) (* 2.0 pi))
        v (Math/sin (* 400.0 r))
        ve (+ (* 0.5 v) 0.5)
        v2 (if (< v 0.5) 0.0 1.0)]
    [v2 v2 v2 1.0]))

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
    
(defn graph2 [w h f]
  (for [y (range h) x (range w)]
    (f [(- (/ (continuous x) w 0.5) 1.0) (- (/ (continuous y) h 0.5) 1.0)])))

(defn pixel-samples
  "generate samples for pixel pxl using sampler-f"
  [sampler-f pxl]
  (map (partial vadd2 pxl) (sampler-f)))

(defn coordinates2 [^long w ^long h]
  "Generate integer coordinates over bounds"
  (for [y (range h) x (range w)] [x y]))

(defn- to-nc [^double x ^double y ^double w ^double h]
  [(- (* (/ x w) 2.0) 1.0) (- (* (/ y h) 2.0) 1.0)])

(defn eval-sample [^Film film ^Filter filter func ^Sample sample]
  (let [^FilmRect film-bounds (.bounds film)
        xy (to-nc (.x-film sample) (.y-film sample)
                  (film-width film-bounds) (film-height film-bounds))
        L (func xy)]
    (film-add-sample film filter (sample-computed sample L))))
 
(defn block-size [n nblocks]
  (if (zero? (mod n nblocks))
    (quot n nblocks)
    (inc (quot n nblocks))))

(defn pgraph [^Film film ^Filter filter sampler-f func nthreads blocks-per-thread]
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
                         (eval-sample film filter func (sample-new xy)))))
                   (partition-all npixels-task
                                  (coordinates2 (film-width bordered)
                                                (film-height bordered))))]
        (doseq [future (.invokeAll pool tasks)]
          (.get future))
        (.shutdown pool)
        (film-write-framebuffer film)))
