(ns cexirt.forge
  (:use cexirt.essentials)
  (:use cexirt.geom)
  (:use cexirt.limath)
  (:use cexirt.film)
  (:use [clojure.pprint :only [pprint]])
  (:require [incanter core charts stats]))

(defn view-scatter2 [xy]
  (incanter.core/view (incanter.charts/scatter-plot (map first xy) (map second xy))))

(defmacro continuous "Discrete to continuous" [^Long d] `(+ ~d 0.5))
(defmacro discrete "Continuous to discrete" [^Double c] `(Math/floor ~c))

(defn stratify1 [w]
  (let [inv-w (double (/ w))]
    (for [x (range w)] (* (+ x (rand)) inv-w))))

(defn stratify2 [w h]
  (let [inv-w (double (/ w)) inv-h (double (/ h))]
    (for [x (range w) y (range h)]
      [ (* (+ x (rand)) inv-w) (* (+ y (rand)) inv-h)])))

;; TODO - this feels icky, but I do like granting all samplers a uniform interface.
(defn sampler-stratify2 [n-samples]
  (let [w (long (Math/ceil (Math/sqrt n-samples)))]
    (partial stratify2 w w)))

(defn uniform1 [w]
  (let [inv-w (double (/ w))]
    (for [x (range w)] (* (+ x 0.5) inv-w))))

(defn uniform2 [w h]
  (let [inv-w (double (/ w)) inv-h (double (/ h))]
    (for [x (range w) y (range h)]
      [ (* (+ x 0.5) inv-w) (* (+ y 0.5) inv-h)])))

(defn sampler-uniform2 [n-samples]
  (let [w (long (Math/ceil (Math/sqrt n-samples)))]
    (partial uniform2 w w)))

(defn box-filter []
  "Box filter"
  (fn [x y] 1.0))

(defn tent-filter [w]
  "Create a tent filter"
  (fn [x y]
    (* (- w (Math/abs x)) (- w (Math/abs y)))))

(defn gaussian-filter [a w]
  (let [exp-w (Math/exp (- (* a w w)))
        gauss1 #(- (Math/exp (- (* a % %))) exp-w)]
        (fn [x y]
          (* (gauss1 x) (gauss1 y)))))
          
(defn gaussian1 [a w x]
  (- (Math/exp (- (* a x x))) (Math/exp (- (* a w w)))))

;; (view-scatter2 (for [x (range -1.0 1.01 0.05)] [x (tent x 0.0)]))
;; (view-scatter2 (for [x (range -2.0 2.01 0.05)] [x (gaussian1 1.0 2.0 x)]))
;; (view-scatter2 (let [f (gaussian-filter 1.0 2.0)] (for [x (range -2.0 2.01 0.2) ] [x (f x 1.)])))

;; TODO - need to expand the framebuffer to accomodate the filter-width

(defn sample-coverage-bounds [^Double sx ^Double sy ^Double filter-width]
  [(- sx filter-width -0.5) (+ sx filter-width -0.5)
   (- sy filter-width -0.5) (+ sy filter-width -0.5)])

(defn sample-coverage-bounds-discrete
  [^Double sx ^Double sy ^Double filter-width]
  [(long (discrete (- sx filter-width -0.5)))
   (long (discrete (+ sx filter-width -0.5)))
   (long (discrete (- sy filter-width -0.5)))
   (long (discrete (+ sy filter-width -0.5)))])

(defn sample-coverage
  "pixels contributed-to by a sample"
  [^Double sx ^Double sy ^Double filter-width]
  (let [B (sample-coverage-bounds-discrete sx sy filter-width)]
    (for [y (range (B 2) (+ (B 3) 1))
          x (range (B 0) (+ (B 1) 1))] [x y])))

(deftype Pixel [xyz weight]
  Object
  (toString [this]
    (str xyz " " weight)))

(defn pixel-add-sample [p xyz weight]
  (Pixel. (vadd3 (.xyz p) (vmul3s xyz weight)) (+ (.weight p) weight)))

(defn pixel-normalize [p]
  (Pixel. (vmul3s (.xyz p) (/ (.weight p))) 1.0))

(defn pixel-null []
  (Pixel. [0.0 0.0 0.0] 0.0))

(defn film-new [w h]
  (vec (for [r (range h)]
              (vec (for [c (range w)]
                     (ref (pixel-null)))))))

(defn film-add-sample [film s x y]
  (dosync
   (commute ((film y) x) pixel-add-sample s 1.0)))

(defn randn [n]
  (+ 1.0 (rand-int n)))
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
        theta (mod (+ t 6.2832) 6.2832)
        v (Math/sin (* 12.0 r))
        ve (+ (* 0.5 v) 0.5)
        v2 (if (< v 0.5) 0.0 1.0)]
    [ve ve ve 1.0]))

(defn sin-theta-f [xy]
  (let [x (xy 0)
        y (xy 1)
        r (max 0.001 (Math/sqrt (+ (* x x) (* y y))))
        t (Math/asin (/ y r))
        theta (mod (+ t 6.2832) 6.2832)
        v (Math/sin (* 12.0 theta))
        ve (+ (* 0.5 v) 0.5)
        v2 (if (< v 0.5) 0.0 1.0)]
    [ve ve ve 1.0]))


(comment (let [w 512 h 256] (finish-framebuffer w h (graph2 w h sin-theta-f))))
(comment (let [w 1280 h 720 f (make-sin-clamped-f)] (finish-framebuffer w h (graph2 w h f))))

(defprotocol Bounding
  (width [this])
  (height [this])
  (depth [this]))

(deftype BoundingBox2 [x0 y0 x1 y1]
  Bounding
  (width [this]
    (- x1 x0))
  (height [this]
    (- y1 y0))
  Object
  (toString [this]
   (str "(" x0 " " y0 ") (" x1 " " y1 ")")))

(defn BoundingBox2-width-height [x y width height]
  (BoundingBox2. x y (+ x width) (+ y height)))

(defn BoundingBox2-new [x0 y0 x1 y1]
  (BoundingBox2. x0 y0 x1 y1))
  
(defn sample-bounds-with-filter [bounds filter-width]
  "Calculate sampling bounds needed to cover the input bounds given a filter-width"
  [(Math/floor (- (continuous (.x0 bounds)) filter-width))
   (Math/floor (- (continuous (.y0 bounds)) filter-width))
   (Math/floor (+ (continuous (.x1 bounds)) filter-width))
   (Math/floor (+ (continuous (.y1 bounds)) filter-width))])
    
(defn graph2 [w h f]
  (for [y (range h) x (range w)]
    (f [(- (/ (continuous x) w 0.5) 1.0) (- (/ (continuous y) h 0.5) 1.0)])))

(defn pixel-samples
  "generate samples for pixel pxl using sampler-f"
  [sampler-f pxl]
  (map (partial vadd2 pxl) (sampler-f)))

(defn image-pixels [w h]
  (for [y (range h) x (range w)] [x y]))

(defn eval-sample [w h f sample]
  (let [to-nc (fn [p] [(- (/ (p 0) w 0.5) 1.0) (- (/ (p 1) h 0.5) 1.0)])
        xy (to-nc sample)
        f-value (f xy)]
    f-value))
    
(defn graph2-aa [w h sampler-f f]
  (map (partial eval-sample w h f)
       (mapcat (partial pixel-samples sampler-f)
               (image-pixels w h))))
