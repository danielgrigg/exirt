(ns cexirt.film
  (:use cexirt.essentials)
  (:use cexirt.limath)
  (:use cexirt.transform)
  (:use cexirt.geom)
  (:use cexirt.filters)
  (:use cexirt.sampling))

(import cexirt.sampling.Sample)
(import cexirt.filters.Filter)

;;(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(deftype FilmRect [^long x0 ^long y0 ^long x1 ^long y1]
  Object
  (toString [this]
    (str "(" x0 " " y0 ") (" x1 " " y1 ")")))

(defn film-rect-width-height [^long x ^long y ^long w ^long h]
  (FilmRect. x y (+ x w) (+ y h)))

(defn film-width [^FilmRect f] (- (.x1 f) (.x0 f)))
(defn film-height [^FilmRect f] (- (.y1 f) (.y0 f)))

(defn clip-film-rect [^FilmRect this ^FilmRect other]
    (FilmRect. (max (.x0 this) (.x0 other))
                   (max (.y0 this) (.y0 other))
                   (min (.x1 this) (.x1 other))
                   (min (.y1 this) (.y1 other))))

(defn film-bounds-with-filter 
  "Calculate sampling bounds needed to cover the input bounds with a filter of width, filter-width"
  [^FilmRect bounds ^double filter-width]
  (FilmRect. (discrete (- (continuous (.x0 bounds)) filter-width))
             (discrete (- (continuous (.y0 bounds)) filter-width))
             (discrete (+ (continuous (.x1 bounds)) filter-width))
             (discrete (+ (continuous (.y1 bounds)) filter-width))))

(defn sample-coverage-bounds
  "Continuous bounding-box of pixels contributed to by sample. Not clipped."
   [^Sample s ^double filter-width]
  [(- (.x-film s) filter-width -0.5)
   (- (.y-film s) filter-width -0.5)
   (+ (.x-film s) filter-width +0.5)
   (+ (.y-film s) filter-width +0.5)])

(defn sample-coverage-bounds-discrete
  "Discrete bounding-box of pixels contributed to by sample. Not clipped."
  [^Sample s ^double filter-width]
  (FilmRect. (long (discrete (- (.x-film s) filter-width -0.5)))
             (long (discrete (- (.y-film s) filter-width -0.5)))
             (long (discrete (+ (.x-film s) filter-width +0.5)))
             (long (discrete (+ (.y-film s) filter-width +0.5)))))

(defn sample-coverage
  "Generate pixels contributed-to by a sample. The pixels are clipped to clip-bounds."
  [^Sample s ^double filter-width ^FilmRect clip-bounds]
  (let [^FilmRect B (clip-film-rect
                     (sample-coverage-bounds-discrete s filter-width)
                     clip-bounds)]
    (for [y (range (.y0 B) (.y1 B))
          x (range (.x0 B) (.x1 B))] [x y])))

(deftype Pixel [xyz ^double weight]
  Object
  (toString [this]
    (str xyz " " weight)))

(defn pixel-add-weighted [^Pixel p sample-xyz ^double sample-weight]
  "Add a weighted sample"
  (Pixel. (vadd3 (.xyz p) (vmul3s sample-xyz sample-weight))
          (+ (.weight p) sample-weight)))

(defn pixel-value [^Pixel p]
  "Normalize a pixel to its weight."
  (if (< (.weight p) 0.0001)
    [0.0 0.0 0.0 1.0]
    (conj (vmul3s (.xyz p) (/ (.weight p))) 1.0)))

(defn pixel-null []
  "Create a blank pixel."
  (atom (Pixel. [0.0 0.0 0.0] 0.0)))

;; NDC to screen coordinates
(defn screen-transform [^double w ^double h]
  (compose (translate 0 h 0) (scale w (- h) 1) (scale 0.5 0.5 1.0) (translate 1 1 0)))

(defn dither [c]
  (let [dk 0.0001
        n4 (vmul4s (conj (vec (rand-gauss2)) (rand-gauss) 0.0) dk)]
    (vadd4 c n4)))

(deftype Film [^FilmRect bounds pixels]
  Object
  (toString [this]
    (str (.bounds this))))

(defn film-new [^FilmRect film-bounds]
  (Film. film-bounds
         (vec (repeatedly
               (* (film-height film-bounds) (film-width film-bounds))
               pixel-null))))

(defn film-pixel [^Film film ^long x ^long y]
  ((.pixels film) (+ (* y (film-width (.bounds film))) x)))

(defn film-framebuffer [^Film film]
  "Write the film contents to a framebuffer"
    (map (fn [pxl] (pixel-value @pxl)) (.pixels film)))

(defn film-add-sample [^Film film ^Filter filter ^Sample sample]
  (doseq [pxl (sample-coverage sample (.width filter) (.bounds film))]
    (let [x-f (- (.x-film sample) (continuous (pxl 0)))
          y-f (- (.y-film sample) (continuous (pxl 1)))
          weight (.evaluate filter x-f y-f)]
      (swap! (film-pixel film (pxl 0) (pxl 1)) 
             pixel-add-weighted (.radiance sample) weight))))

(defn- framebuffer-native [fb]
  (float-array (apply concat fb)))

(defn framebuffer-finish [^Film film out-path]
  (jna-call :exr_basic "write_rgba"
            Integer (film-width (.bounds film)) (film-height (.bounds film))
            out-path
            (framebuffer-native
             (film-framebuffer film))))
