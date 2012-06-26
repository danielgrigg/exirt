(ns cexirt.sampling
  (:use cexirt.essentials)
  (:use cexirt.limath))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(deftype Sample [^double x-film ^double y-film radiance]
  Object
  (toString [this]
    (str "x-film " x-film " y-film " y-film " value " radiance)))

(defn sample-new [film-xy]
  (Sample. (film-xy 0) (film-xy 1) [0.0 0.0 0.0]))

(defn sample-radiance [^Sample s L]
  (Sample. (.x-film s) (.y-film s) L))

(defn stratify1 [^long w]
  (let [inv-w (double (/ w))]
    (for [x (range w)] (* (+ x (rand)) inv-w))))

(defn stratify2 [^long w ^long h]
  (let [inv-w (double (/ w)) inv-h (double (/ h))]
    (for [x (range w) y (range h)]
      [ (* (+ x (rand)) inv-w) (* (+ y (rand)) inv-h)])))

(defn sampler-stratify2 [^long n-samples]
  (let [w (long (Math/ceil (Math/sqrt n-samples)))]
    (partial stratify2 w w)))

(defn uniform1 [^long w]
  (let [inv-w (double (/ w))]
    (for [x (range w)] (* (+ x 0.5) inv-w))))

(defn uniform2 [^long w ^long h]
  (let [inv-w (double (/ w)) inv-h (double (/ h))]
    (for [x (range w) y (range h)]
      [ (* (+ x 0.5) inv-w) (* (+ y 0.5) inv-h)])))

(defn sampler-uniform2 [^long n-samples]
  (let [w (long (Math/ceil (Math/sqrt n-samples)))]
    (partial uniform2 w w)))
          
