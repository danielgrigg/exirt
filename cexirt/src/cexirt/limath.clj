(ns cexirt.limath
  (:require [clojure.math.numeric-tower :as numeric])
  (:use cexirt.essentials)
  (:use [clojure.pprint :only [pprint]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:const pi Math/PI)
(def ^:const eps 4e-5)
(def ^:const eps-small 1.4E-45)
(def ^:const infinity 1e37)

(defmacro sq [x]
  `(* ~x ~x))

(defn equal-scalar
  ([^double x ^double y]
     (< (numeric/abs (- x y)) eps))
  ([^double x ^double y ^double tolerance]
     (< (numeric/abs (- x y)) tolerance)))

(defn rand-gauss []
  (let [u (rand)
        v (rand)]
    (* (numeric/sqrt (* -2.0 (Math/log u))) (Math/cos (* 2.0 pi v)))))

(defn rand-gauss2 []
  (let [u (rand)
        v (rand)]
    [(* (numeric/sqrt (* -2.0 (Math/log u))) (Math/cos (* 2.0 pi v)))
     (* (numeric/sqrt (* -2.0 (Math/log u))) (Math/sin (* 2.0 pi v)))]))

(defn quadratic [^double A ^double B ^double C]
  (let [discrim (- (* B B) (* 4.0 A C))]
    (if (< discrim 0)
      nil
      (let [rootDiscrim (numeric/sqrt discrim)
            q (if (< B 0)
                (* -0.5 (- B rootDiscrim))
                (* -0.5 (+ B rootDiscrim)))
            t0 (/ q A)
            t1 (/ C q)]
        (if (> t0 t1)
          [t1 t0]
          [t0 t1])))))

(defmacro vec-op [op n & args]
  `[~@(for [x (range n)]
        `(~op ~@(for [y args] `(~y ~x))))])

(defmacro vec-scalar-op [op n v scalar]
  `[~@(for [x (range n)]
        `(~op (~v ~x) ~scalar))])

(defmacro vec-reduce [rop mop n & args]
  `(~rop ~@(for [x (range n)]
             `(~mop ~@(for [y args] `(~y ~x))))))

(defmacro make-vec-unary-ops [name desc op]
  (cons `do
        (for [n [2 3 4]]
          `(defn ~(str-sym \v name n)
             ~(str desc " a " n "-vector")
             [~'a]
             (vec-op ~op ~n ~'a)))))

;; generates vector functions for 2, 3 & 4 component vectors.
(defmacro make-vec-ops [name desc op self-op]
  (cons `do
        (for [n [2 3 4]]
          `(defn ~(str-sym \v name n)
             ~(str desc n "-vector(s)")
             ~(if self-op ['a] ['a 'b])
             (vec-op ~op ~n ~@(if self-op ['a 'a] ['a 'b]))))))

(defmacro make-vec-scalar-ops [name desc op]
  (cons `do
        (for [n [2 3 4]]
          `(defn ~(str-sym \v name n "s")
             ~(str desc " vector v and scalar s")
             [~'a ~'k]
             (vec-scalar-op ~op ~n ~'a ~'k)))))

(defmacro make-vec-reduce-ops [name desc rop mop self-op]
  (cons `do
        (for [n [2 3 4]]
          `(defn ~(str-sym \v name n)
             ~(str desc " " n "-vector(s)")
             ~(if self-op ['a] ['a 'b])          
             (vec-reduce ~rop ~mop ~n ~@(if self-op ['a 'a] ['a 'b]))))))


(make-vec-ops add "Add" + false)
(make-vec-ops sub "Subtract" - false)
(make-vec-ops mul "Multiply" * false)
(make-vec-ops div "Divide" / false)
(make-vec-ops max "Maximum" max false)
(make-vec-ops min "Minimum" min false)
(make-vec-reduce-ops dot "Dot product" + * false)
(make-vec-reduce-ops length-sq "Length^2" + * true)
(make-vec-scalar-ops add "Add" +)
(make-vec-scalar-ops sub "Subtract" -)
(make-vec-scalar-ops mul "Multiply" *)
(make-vec-scalar-ops div "Divide" /)
(make-vec-reduce-ops equal "Equality" and equal-scalar false)
(make-vec-unary-ops floor "floor" numeric/floor)
(make-vec-unary-ops ceil "ceil" numeric/ceil)
(make-vec-unary-ops abs "abs" numeric/abs)

(defmacro make-vec-length []
  (cons `do
        (for [n [2 3 4]]
          `(defn ~(str-sym \v "length" n)
             ~(str "Length of " n "-vector") [~'a]
             (numeric/sqrt (~(str-sym "vlength-sq" n) ~'a))))))

(make-vec-length)             

(defn vnormalize2 [a] (vmul2s a (/ (max eps (vlength2 a)))))
(defn vnormalize3 [a] (vmul3s a (/ (max eps (vlength3 a)))))
(defn vnormalize4 [a] (vmul4s a (/ (max eps (vlength4 a)))))

(defn cross [a b]
  [(- (* (a 1) (b 2)) (* (a 2) (b 1)))
   (- (* (a 2) (b 0)) (* (a 0) (b 2)))
   (- (* (a 0) (b 1)) (* (a 1) (b 0)))])

(defn xyz [v] [(v 0) (v 1) (v 2)])

;; MATRIX OPERATIONS
;; Genereate per-element matrix functions
(defmacro make-matrix-ops [op-name desc]
  (cons `do
        (for [n [2 3 4]]
          `(defn ~(str-sym "ms" op-name n)
             ~(str desc " two " n "-matrices")
             [~'A ~'B]
             [~@(for [r (range n)] `(~(str-sym \v op-name n) (~'A ~r) (~'B ~r)))]))))

(defmacro make-matrix-scalar-ops [op-name desc]
  (cons `do
        (for [n [2 3 4]]
          `(defn ~(str-sym "m" op-name n "s")
             ~(str desc " matrix with scalar")
             [~'A ~'k]
             [~@(for [r (range n)] `(~(str-sym \v op-name n "s") (~'A ~r) ~'k))]))))


(defmacro make-mmul []
  (cons 'do
        (for [n [2 3 4]]
          (let [mmul# (str-sym "mmul" n)]
          `(defn ~mmul#  ~(str "Multiply two " n "x" n " matrices")
             ([~'A ~'B]
                (let [~@(apply concat
                               `(~@(for [c (range n)]
                                     `[~(str-sym "c" c)
                                       (~(str-sym "mcol" n) ~'B ~c) ])))]
                  [~@(for [x (range n)]
                       (vec (for [y (range n)]
                              `(~(str-sym "vdot" n) (~'A ~x) ~(str-sym "c" y)))))]))
             ([~'A ~'B & ~'xs]
                (reduce ~mmul# (~mmul# ~'A ~'B) ~'xs)))))))

(make-matrix-ops add "Add")
(make-matrix-ops sub "Subtract")
(make-matrix-ops mul "Multiply")
(make-matrix-ops div "Divide")
(make-matrix-scalar-ops add "Add")
(make-matrix-scalar-ops sub "Subtract")
(make-matrix-scalar-ops mul "Multiply")
(make-matrix-scalar-ops div "Divide")

;; transform v by M
(defn mvmul2 "transform 2-vector v by M" [M v]
  [(vdot2 (M 0) v) (vdot2 (M 1) v)])
(defn mvmul3 "transform 3-vector v by M" [M v]
  [(vdot3 (M 0) v) (vdot3 (M 1) v) (vdot3 (M 2) v)])
(defn mvmul4 "transform 4-vector v by M" [M v]
  [(vdot4 (M 0) v) (vdot4 (M 1) v) (vdot4 (M 2) v) (vdot4 (M 3) v)])

(defn mvmul34 "transform 4-vector ignoring bottom row" [M v]
  [(vdot4 (M 0) v) (vdot4 (M 1) v) (vdot4 (M 2) v) (v 3)])

(defn mrow [M r] (M r))
(defn mcol2 [M c] [((M 0) c) ((M 1) c)])
(defn mcol3 [M c] [((M 0) c) ((M 1) c) ((M 2) c)])
(defn mcol4 [M c] [((M 0) c) ((M 1) c) ((M 2) c) ((M 3) c)])

(make-mmul)

(defn mtranspose3 "2x2 transpose"
  [A] [(mcol2 A 0) (mcol2 A 1)])
(defn mtranspose3 "3x3 transpose"
  [A] [(mcol3 A 0) (mcol3 A 1) (mcol3 A 2)])
(defn mtranspose4 "4x4 transpose"
  [A] [(mcol4 A 0) (mcol4 A 1) (mcol4 A 2) (mcol4 A 3)])

(defmacro make-matrix-lookups []
  (cons 'do
        (for [r (range 4) c (range 4)]
          `(defn ~(str-sym "m" r c)
             [~'A]
             ((~'A ~r) ~c)))))

(make-matrix-lookups)

(defn mdet2 "2x2 matrix determinant" [A]
  (- (* (m00 A) (m11 A)) (* (m01 A) (m10 A))))

(defn mdet3 "3x3 matrix determinant" [M]
  (vdot3 (mrow M 0)
         [(- (* (m11 M) (m22 M)) (* (m12 M) (m21 M)))
          (- (* (m12 M) (m20 M)) (* (m22 M) (m10 M)))
          (- (* (m10 M) (m21 M)) (* (m11 M) (m20 M)))]))

(defn minverse2 [A]
  (mmul2s [[(m11 A) (-(m01 A))] [(-(m10 A)) (m00 A)]]
          (/ (double (mdet2 A)))))

(defn minverse3 [M]
  "3x3 matrix inverse"
  (let [A (- (* (m11 M) (m22 M)) (* (m12 M) (m21 M))) 
        B (- (* (m12 M) (m20 M)) (* (m10 M) (m22 M)))
        C (- (* (m10 M) (m21 M)) (* (m11 M) (m20 M))) 
        D (- (* (m02 M) (m21 M)) (* (m01 M) (m22 M))) 
        E (- (* (m00 M) (m22 M)) (* (m02 M) (m20 M))) 
        F (- (* (m20 M) (m01 M)) (* (m00 M) (m21 M))) 
        G (- (* (m01 M) (m12 M)) (* (m02 M) (m11 M)))
        H (- (* (m02 M) (m10 M)) (* (m00 M) (m12 M)))                                                  
        K (- (* (m00 M) (m11 M)) (* (m01 M) (m10 M)))]
    (mmul3s [[A D G]
             [B E H]
             [C F K]] (/ (double (mdet3 M))))))

(defn minverse4 [M]
  (let [t [(* (m22 M) (m33 M))           (* (m23 M) (m32 M))
           (* (m21 M) (m33 M))           (* (m23 M) (m31 M))
           (* (m21 M) (m32 M))           (* (m22 M) (m31 M))
           (* (m20 M) (m33 M))           (* (m23 M) (m30 M))
           (* (m20 M) (m32 M))           (* (m22 M) (m30 M))
           (* (m20 M) (m31 M))           (* (m21 M) (m30 M))
           (* (m02 M) (m13 M))           (* (m03 M) (m12 M))
           (* (m01 M) (m13 M))           (* (m03 M) (m11 M))
           (* (m01 M) (m12 M))           (* (m02 M) (m11 M))
           (* (m00 M) (m13 M))           (* (m03 M) (m10 M))
           (* (m00 M) (m12 M))           (* (m02 M) (m10 M))
           (* (m00 M) (m11 M))           (* (m01 M) (m10 M))]

        B [[(- (+ (* (t 0) (m11 M)) (* (t 3) (m12 M)) (* (t 4) (m13 M)))
               (+ (* (t 1) (m11 M)) (* (t 2) (m12 M)) (* (t 5) (m13 M))))         
            (- (+ (* (t 1) (m01 M)) (* (t 2) (m02 M)) (* (t 5) (m03 M)))
               (+ (* (t 0) (m01 M)) (* (t 3) (m02 M)) (* (t 4) (m03 M))))
            (- (+ (* (t 12) (m31 M)) (* (t 15) (m32 M)) (* (t 16) (m33 M)))
               (+ (* (t 13) (m31 M)) (* (t 14) (m32 M)) (* (t 17) (m33 M))))
            (- (+ (* (t 14) (m22 M)) (* (t 17) (m23 M)) (* (t 13) (m21 M)))
               (+ (* (t 16) (m23 M)) (* (t 12) (m21 M)) (* (t 15) (m22 M))))]

           [(- (+ (* (t 1) (m10 M)) (* (t 6) (m12 M)) (* (t 9) (m13 M)))
               (+ (* (t 0) (m10 M)) (* (t 7) (m12 M)) (* (t 8) (m13 M))))          
            (- (+ (* (t 0) (m00 M)) (* (t 7) (m02 M)) (* (t 8) (m03 M)))
               (+ (* (t 1) (m00 M)) (* (t 6) (m02 M)) (* (t 9) (m03 M))))          
            (- (+ (* (t 13) (m30 M)) (* (t 18) (m32 M)) (* (t 21) (m33 M)))
               (+ (* (t 12) (m30 M)) (* (t 19) (m32 M)) (* (t 20) (m33 M))))            
            (- (+ (* (t 20) (m23 M)) (* (t 12) (m20 M)) (* (t 19) (m22 M)))
               (+ (* (t 18) (m22 M)) (* (t 21) (m23 M)) (* (t 13) (m20 M))))]

           [(- (+ (* (t 2) (m10 M)) (* (t 7) (m11 M)) (* (t 10) (m13 M)))
               (+ (* (t 3) (m10 M)) (* (t 6) (m11 M)) (* (t 11) (m13 M))))          
            (- (+ (* (t 3) (m00 M)) (* (t 6) (m01 M)) (* (t 11) (m03 M)))
               (+ (* (t 2) (m00 M)) (* (t 7) (m01 M)) (* (t 10) (m03 M))))          
            (- (+ (* (t 14) (m30 M)) (* (t 19) (m31 M)) (* (t 22) (m33 M)))
               (+ (* (t 15) (m30 M)) (* (t 18) (m31 M)) (* (t 23) (m33 M))))            

            (- (+ (* (t 18) (m21 M)) (* (t 23) (m23 M)) (* (t 15) (m20 M)))
               (+ (* (t 22) (m23 M)) (* (t 14) (m20 M)) (* (t 19) (m21 M))))            
            ]

           [(- (+ (* (t 5) (m10 M)) (* (t 8) (m11 M)) (* (t 11) (m12 M)))
               (+ (* (t 4) (m10 M)) (* (t 9) (m11 M)) (* (t 10) (m12 M))))
            (- (+ (* (t 4) (m00 M)) (* (t 9) (m01 M)) (* (t 10) (m02 M)))
               (+ (* (t 5) (m00 M)) (* (t 8) (m01 M)) (* (t 11)(m02 M))))
            (- (+ (* (t 17) (m30 M)) (* (t 20) (m31 M)) (* (t 23) (m32 M)))
               (+ (* (t 16) (m30 M)) (* (t 21) (m31 M)) (* (t 22) (m32 M))))
            (- (+ (* (t 22) (m22 M)) (* (t 16) (m20 M)) (* (t 21) (m21 M)))
               (+ (* (t 20) (m21 M)) (* (t 23) (m22 M)) (* (t 17) (m20 M))))]]]
    (mmul4s B (/ (double (vdot4 (mrow M 0) (mcol4 B 0)))))))

(defn midentity2 [] [[1 0] [0 1]])
(defn midentity3 [] [[1 0 0] [0 1 0] [0 0 1]])
(defn midentity4 [] [[1 0 0 0] [0 1 0 0] [0 0 1 0] [0 0 0 1]])

(defmacro make-matrix-generator [f is-fn]
  (cons `do
        (for [n [2 3 4]]
          `(defn ~(str-sym "m" f n)
             ~(str "Generate " n "x" n " matrix with entries generated by f")
             []
             [~@(for [r (range n)]
                  `[~@(for [c (range n)] (if is-fn `(~f) f) )])]))))

(make-matrix-generator rand true)

(defn mequal2 [A B] (every? true? (map vequal2 A B)))
(defn mequal3 [A B] (every? true? (map vequal3 A B)))
(defn mequal4 [A B] (every? true? (map vequal4 A B)))
