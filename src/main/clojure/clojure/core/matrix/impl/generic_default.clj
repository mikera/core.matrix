(ns clojure.core.matrix.impl.generic-default
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.generic-protocols :as gmp]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.impl.wrappers :as wrap]
            [clojure.core.matrix.multimethods :as mm]
            [clojure.core.matrix.impl.mathsops :as mops]
            [clojure.core.matrix.impl.double-array]
            [clojure.core.matrix.implementations :as imp]))


(defmacro array?
  "Returns true if the parameter is an N-dimensional array of any type"
  ([m]
    `(not (mp/is-scalar? ~m))))

(defn- square?
  "Returns true if matrix is square (2D with same number of rows and columns)"
  ([m]
    (and
      (== 2 (mp/dimensionality m))
      (== (mp/dimension-count m 0) (mp/dimension-count m 1)))))

(defn- calc-element-count
  "Returns the total count of elements in an array"
  ([m]
    (cond
      (array? m) (reduce * 1 (mp/get-shape m))
      :else (count m))))

;; TODO: make smarter for different numeric types
(defn construct-mutable-matrix
  "Constructs a new mutable matrix with the given data."
  ([m]
    (let [dims (long (mp/dimensionality m))
          type (mp/element-type m)
          double? (or (= Double/TYPE type))]
      (cond
        (== dims 0)
          (wrap/wrap-scalar (mp/get-0d m))
        (and (== dims 1) double?)
          (clojure.core.matrix.impl.double-array/construct-double-array m)
        double?
          (mp/coerce-param (imp/get-canonical-object :ndarray-double) m)
        :else
          (mp/coerce-param (imp/get-canonical-object :ndarray) m)))))



(extend-protocol gmp/PGenericMatrixMultiply
  Object
  (generic-matrix-multiply [m a spec]
    (let [scalar? (:scalar? spec) mul (:mul spec) add (:add spec)]
      (if (scalar? spec)
        (if (number? a)
          (mul m a)
          (gmp/generic-pre-scale a m spec))
        (let [mdims (long (mp/dimensionality m))
              adims (long (mp/dimensionality a))]
          (cond
           (== adims 0) (gmp/generic-scale m a spec)
           (and (== mdims 1) (== adims 2))
           (let [[arows acols] (mp/get-shape a)]
             (mp/reshape (gmp/generic-matrix-multiply
                          (mp/reshape m [1 arows]) a spec)
                         [arows]))
           (and (== mdims 2) (== adims 1))
           (let [[mrows mcols] (mp/get-shape m)]
             (mp/reshape (gmp/generic-matrix-multiply
                          m (mp/reshape a [mcols 1]) spec)
                         [mcols]))
           (and (== mdims 2) (== adims 2))
           (let [mutable (mp/is-mutable? m)
                 [mrows mcols] (mp/get-shape m)
                 [arows acols] (mp/get-shape a)
                 new-m-type (if mutable m (imp/get-canonical-object :ndarray))
                 new-m (mp/new-matrix new-m-type mrows acols)]
             (do
               ;; TODO: optimize cache-locality (http://bit.ly/12FgFbl)
               (c-for [i (long 0) (< i mrows) (inc i)
                       j (long 0) (< j acols) (inc j)]
                 (mp/set-2d! new-m i j 0))
               (c-for [i (long 0) (< i mrows) (inc i)
                       j (long 0) (< j acols) (inc j)
                       k (long 0) (< k mcols) (inc k)]
                 (mp/set-2d! new-m i j (add (mp/get-2d new-m i j)
                                        (mul (mp/get-2d m i k)
                                         (mp/get-2d a k j)))))
               new-m)))))))
  (generic-element-multiply [m a spec]
    (let [scalar? (:scalar? spec) mul (:mul spec)]
      (if (scalar? m)
        (cond
         (scalar? a) (mul m a)
         (array? a) (gmp/generic-pre-scale a m spec)
         :else (error "Don't know how to multiply number with: " (class a)))
        (if (scalar? a)
          (gmp/generic-scale m a spec)
          (let [[m a] (mp/broadcast-compatible m a)]
            (mp/element-map m mul a)))))))

;; matrix multiply
(extend-protocol gmp/PGenericMatrixMultiplyMutable
  Object
  (generic-element-multiply! [m a spec]
    (let [scalar? (:scalar? spec) mul (:mul spec)]
      (if (scalar? m)
        (error "Can't do mutable multiply on a scalar number")
        (mp/element-map! m mul (mp/broadcast-like m a)))))
  (generic-matrix-multiply! [m a spec]
    (let [scalar? (:scalar? spec)]
      (if (scalar? m)
        (error "Can't do mutable multiply on a scalar number")
        (mp/assign! m (gmp/generic-matrix-multiply m a spec))))))

(extend-protocol gmp/PGenericMatrixDivide
  Object
    (generic-element-divide
      ([m spec] (let [scalar? (:scalar? spec) div (:div spec)]
                  (if (scalar? m)
                    (div m)
                    (mp/element-map m #(div m %)))))
      ([m a spec]
         (let [scalar? (:scalar? spec) div (:div spec)]
           (if (scalar? m)
             (mp/element-map a #(div m %))
             (let [[m a] (mp/broadcast-compatible m a)]
               (mp/element-map m #(div %1 %2) a)))))))

(extend-protocol gmp/PGenericMatrixDivideMutable
  Object
  (generic-element-divide!
    ([m spec] (let [scalar? (:scalar? spec) div (:div spec)]
                (if (scalar? m)
                  (error "Can't do mutable divide on a scalar number")
                  (mp/element-map! m #(div %)))))
    ([m a spec]
       (let [scalar? (:scalar? spec) div (:div spec)]
         (if (scalar? m)
           (error "Can't do mutable divide on a scalar numer")
           (let [[m a] (mp/broadcast-compatible m a)]
             (mp/element-map! m #(div %1 %2) a)))))))

;; matrix element summation
(extend-protocol gmp/PGenericSummable
  Object
  (generic-element-sum [a spec]
    (let [scalar? (:scalar? spec) add (:add spec)]
      (if (scalar? a)
        a
        (mp/element-reduce a add)))))

(extend-protocol gmp/PGenericElementMinMax
  Object
  (generic-element-min [m spec]
    (let [scalar? (:scalar? spec) < (:< spec)]
      (if (scalar? m)
        m
        (mp/element-reduce m 
                           (fn [best v] (if (or (not best) (< v best)) v best)) 
                           nil))))
  (generic-element-max [m spec]
    (let [scalar? (:scalar? spec) > (:> spec)]
      (if (scalar? m)
        m
        (mp/element-reduce m 
                           (fn [best v] (if (or (not best) (> v best)) v best)) 
                           nil)))))

;; add-product operations
(extend-protocol gmp/PGenericAddProduct
  Object
  (generic-add-product [m a b spec]
    (let [scalar? (:scalar? m) add (:add spec) mul (:mul spec)]
      (if (scalar? m)
        (add m (mul a b))
        (gmp/generic-matrix-add
         m (gmp/generic-element-multiply a b spec) spec)))))

(extend-protocol gmp/PGenericAddProductMutable
  Object
  (generic-add-product! [m a b spec]
    (let [scalar? (:scalar? spec)]
      (if (scalar? m)
        (error "Scalars are not mutable")
        (gmp/generic-matrix-add! m (gmp/generic-element-multiply a b spec)
                                 spec)))))

(extend-protocol gmp/PGenericAddScaled
  Object
  (generic-add-scaled [m a factor spec]
    (let [scalar? (:scalar? spec) add (:add spec) mul (:mul spec)]
      (if (scalar? m)
        (add m (mul a factor))
        (gmp/generic-matrix-add m (gmp/generic-scale a factor spec) spec)))))

(extend-protocol gmp/PGenericAddScaledMutable
  Object
  (generic-add-scaled! [m a factor spec]
    (let [scalar? (:scalar? spec)]
      (if (scalar? m)
        (error "Scalars are not mutable")
        (gmp/generic-matrix-add! m (gmp/generic-scale a factor spec) spec)))))

(extend-protocol gmp/PGenericAddScaledProduct
  Object
  (generic-add-scaled-product [m a b factor spec]
    (let [scalar? (:scalar? spec) add (:add spec) mul (:mul spec)]
      (if (scalar? m)
        (add m (mul a b factor))
        (gmp/generic-matrix-add
         m (gmp/generic-scale
            (gmp/generic-element-multiply a b spec) factor spec) spec)))))

(extend-protocol gmp/PGenericAddScaledProductMutable
  Object
  (generic-add-scaled-product! [m a b factor spec]
    (let [scalar? (:scalar? spec)]
      (if (scalar? m)
        (error "Scalars are not mutable")
        (gmp/generic-matrix-add!
         m (gmp/generic-scale
            (gmp/generic-element-multiply a b spec) factor spec) spec)))))

(extend-protocol gmp/PGenericVectorTransform
  clojure.lang.IFn
    (generic-vector-transform [m a spec]
      (if (vector? m) (gmp/generic-matrix-multiply m a spec)
          ;;TODO should here spec be passed? 
          (m a)))
    (generic-vector-transform! [m a spec]
      (if (vector? m) (mp/assign! a (gmp/generic-matrix-multiply m a spec))
          ;;TODO should here spec be passed?
          (mp/assign! a (m a))))
  Object
    (generic-vector-transform [m a spec]
      (cond
        (== 2 (mp/dimensionality m)) (gmp/generic-matrix-multiply m a spec)
        :else (error "Don't know how to transform using: " (class m))))
    (generic-vector-transform! [m a spec]
      (mp/assign! a (gmp/generic-vector-transform m a spec))))


;; matrix scaling
(extend-protocol gmp/PGenericMatrixScaling
  Object
  (generic-scale [m a spec]
    (let [scalar? (:scalar? spec) mul (:mul spec)]
      (if (scalar? m)
        (if (scalar? a)
          (mul m a)
          (gmp/generic-pre-scale a m spec))
        (mp/element-map m #(mul % a)))))
  (generic-pre-scale [m a spec]
    (let [scalar? (:scalar? spec) mul (:mul spec)]
      (if (scalar? m)
        (if (scalar? a)
          (mul a m)
          (gmp/generic-scale a m spec))
        (mp/element-map m (partial mul a))))))

(extend-protocol gmp/PGenericMatrixMutableScaling
  Object
  (generic-scale! [m a spec]
    (let [scalar? (:scalar? spec) mul (:mul spec)]
      (if (scalar? m)
        (error "Can't scale! a scalar: " m)        
        (do (mp/element-map! m #(mul % a))
            m))))
  (generic-pre-scale! [m a spec]
    (let [scalar? (:scalar? spec) mul (:mul spec)]
      (if (scalar? m)
        (error "Can't pre-scale! a scalar: " m)
        (do (mp/element-map! m (partial mul a))
            m)))))

(extend-protocol gmp/PGenericMatrixAdd
  ;; matrix add for scalars
  ;; default impelementation - assume we can use emap?
  Object
  (generic-matrix-add [m a spec]
    (let [scalar? (:scalar? spec) add (:add spec)]
      (if (scalar? m)
        (if (scalar? a) (add m a)
            (gmp/generic-matrix-add a m spec))
        (let [[m a] (mp/broadcast-compatible m a)]
          (mp/element-map m add a)))))
  (generic-matrix-sub [m a spec]
    (let [scalar? (:scalar? spec) sub (:sub spec)]
      (if (scalar? m)
        (if (scalar? a) (sub m a)
            (gmp/generic-negate (gmp/generic-matrix-sub a m spec) spec))
        (let [[m a] (mp/broadcast-compatible m a)]
          (mp/element-map m sub a))))))

(extend-protocol gmp/PGenericMatrixAddMutable
  ;; matrix add for scalars
  Number
    (generic-matrix-add! [m a spec]
)
    (generic-matrix-sub! [m a spec]
)
  ;; default impelementation - assume we can use emap?
  Object
  (generic-matrix-add! [m a spec]
    (let [scalar? (:scalar? spec) add (:add spec)]
      (if (scalar? m)
        (error "Can't do mutable add! on a scalar")
        (mp/element-map! m add a))))
  (generic-matrix-sub! [m a spec]
    (let [scalar? (:scalar? spec) sub (:sub spec)]
      (if (scalar? m)
        (error "Can't do mutable sub! on a scalar")
        (mp/element-map! m sub a)))))

(extend-protocol gmp/PGenericNegation
  nil
  (generic-negate [m spec]
    (error "Can't negate nil!"))
  Object
  (generic-negate [m spec]
    (let [scalar? (:scalar? spec) sub (:sub spec) one (:one spec)]
      (if (scalar? m)
        (sub m)
        (gmp/generic-scale m (sub one) spec)))))

;; equality checking
(extend-protocol gmp/PGenericMatrixEquality
  nil
    (generic-matrix-equals [a b spec]
      (error "nil is not a valid numerical value in equality testing"))
  Object
  (generic-matrix-equals [a b spec]
    (let [scalar? (:scalar? spec) = (:= spec)]
      (if (scalar? a)
        (cond
         (scalar? b) (= a b)
         (== 0 (mp/dimensionality b)) (= a (scalar-coerce b))
         :else false)
        (cond
         (identical? a b) true
         (mp/same-shape? a b)
         (if (== 0 (mp/dimensionality a))
           (= (mp/get-0d a) (scalar-coerce b))
           (not (some false? (map = (mp/element-seq a) (mp/element-seq b)))))
         :else false)))))

(extend-protocol gmp/PGenericValueEquality
  nil
    (generic-value-equals [a b spec]
      (or 
        (nil? b)
        (and 
          (== 0 (mp/dimensionality b))
          (nil? (mp/get-0d b)))))
  Object
  (generic-value-equals [a b spec]
    (let [= (:= spec)]
      (and
       (mp/same-shape? a b)
       (every? true? (map = (mp/element-seq a) (mp/element-seq b)))))))

(defmacro eps== [a b eps]
  `(<= (~'abs (~'sub ~a ~b)) (double ~eps)))

;; equality checking
(extend-protocol gmp/PGenericMatrixEqualityEpsilon
  nil
    (generic-matrix-equals-epsilon [a b eps spec]
      (error "nil is not a valid numerical value in equality testing"))
  Object
  (generic-matrix-equals-epsilon [a b eps spec]
    (let [scalar? (:scalar? spec) abs (:abs spec) sub (:sub spec)]
      (if (scalar? a)
        (cond
         (scalar? b) (eps== a b eps)
         (== 0 (mp/dimensionality b)) (eps== a (mp/get-0d b) eps)
         :else false)
        (cond
         (identical? a b) true
         (mp/same-shape? a b)
         (let [eps (double eps)]
           (every? #(<= (abs %) eps) (map sub (mp/element-seq a)
                                          (mp/element-seq b))))
         :else false)))))



(extend-protocol gmp/PGenericVectorOps
  Object
  (generic-vector-dot [a b spec]
    (let [scalar? (:scalar? spec)]
      (if (scalar? a)
        (gmp/generic-pre-scale b a spec)
        (gmp/generic-element-sum (gmp/generic-element-multiply a b spec) spec))))
  (generic-length [a spec]
    (let [scalar? (:scalar? spec) sqrt (:sqrt spec)]
      (if (scalar? a)
        a
        (sqrt (gmp/generic-length-squared a spec)))))
  (generic-length-squared [a spec]
    (let [scalar? (:scalar? spec) add (:add spec) mul (:mul spec)
          sqrt (:sqrt spec)]
      (if (scalar? a)
        (sqrt a)
        (mp/element-reduce a (fn [r x] (add r (mul x x))) 0))))
  (generic-normalise [a spec]
    (let [scalar? (:scalar spec) div (:div spec) one (:one spec) sub (:sub spec)
          sqrt (:sqrt spec) < (:< spec) > (:> spec) zero (:zero spec)]
      (if (scalar? a)
        (cond
         (> a zero) one
         (< a zero) (sub one)
         :else zero)
        (gmp/generic-scale a (div one (sqrt (gmp/generic-length-squared a spec)))
                           spec)))))

(extend-protocol gmp/PGenericVectorDistance
  Object
  (generic-distance [a b spec]
    (let [scalar? (:scalar? spec) abs (:abs spec) sub (:sub spec)]
      (if (scalar? a)
        (abs (sub b a))
        (gmp/generic-length (gmp/generic-matrix-sub a b spec) spec)))))

(extend-protocol gmp/PGenericVectorCross
  Object
  (generic-cross-product [a b spec]
    (let [sub (:sub spec) mul (:mul spec)]
      (let [x1 (mp/get-1d a 0)
            y1 (mp/get-1d a 1)
            z1 (mp/get-1d a 2)
            x2 (mp/get-1d b 0)
            y2 (mp/get-1d b 1)
            z2 (mp/get-1d b 2)]
        (mp/construct-matrix a [(sub (mul y1 z2) (mul z1 y2))
                                (sub (mul z1 x2) (mul x1 z2))
                                (sub (mul x1 y2) (mul y1 x2))]))))
  (generic-cross-product! [a b spec]
    (let [sub (:sub spec) mul (:mul spec)]
      (let [x1 (mp/get-1d a 0)
            y1 (mp/get-1d a 1)
            z1 (mp/get-1d a 2)
            x2 (mp/get-1d b 0)
            y2 (mp/get-1d b 1)
            z2 (mp/get-1d b 2)]
        (mp/set-1d! a 0 (sub (mul y1 z2) (mul z1 y2)))
        (mp/set-1d! a 1 (sub (mul z1 x2) (mul x1 z2)))
        (mp/set-1d! a 2 (sub (mul x1 y2) (mul y1 x2)))
        a))))

(extend-protocol gmp/PGenericMutableVectorOps
  Object
  (generic-normalise! [a spec]
    (let [one (:one spec) div (:div spec) sqrt (:sqrt spec)]
      (gmp/generic-scale! a (div 1.0 (sqrt (gmp/generic-length-squared a spec)))
                          spec))))

(extend-protocol gmp/PGenericMatrixOps
  nil
    (generic-trace [m spec] m)
  Object
  (generic-trace [m spec]
    (let [scalar? (:scalar? spec) add (:add spec)]
      (if (scalar? m)
        m
        (do (when-not (== 2 (mp/dimensionality m)) (error "Trace requires a 2D matrix"))
            (let [rc (mp/dimension-count m 0)
                  cc (mp/dimension-count m 1)
                  dims (long rc)]
              (when-not (== rc cc) (error "Can't compute trace of non-square matrix"))
              (loop [i 0 res 0.0]
                (if (>= i dims)
                  res
                  (recur (inc i) (add res (mp/get-2d m i i)))))))))))

(extend-protocol gmp/PGenericMatrixProducts
  Number
    (inner-product [m a]
      )
    (outer-product [m a]
      )
  Object
  (generic-inner-product [m a spec]
    (let [scalar? (:scalar? spec) mul (:mul spec)]
      (if (scalar? m)
        (if (scalar? a)
        (mul m a)
        (gmp/generic-pre-scale a m spec))
        (cond
         (scalar? m)
         (gmp/generic-pre-scale a m spec)
         (scalar? a)
         (gmp/generic-scale m a spec)
         (== 1 (mp/dimensionality m))
         (if (== 1 (mp/dimensionality a))
           (gmp/generic-element-sum (gmp/generic-element-multiply m a spec) spec)
           (reduce #(gmp/generic-matrix-add %1 %2 spec)
                   (map (fn [sl x] (gmp/generic-scale sl x spec))
                        (mp/get-major-slice-seq a)
                        (mp/get-major-slice-seq m)))) ;; TODO: implement with mutable accumulation
         :else
         (mapv #(gmp/generic-inner-product % a spec)
               (mp/get-major-slice-seq m))))))
  (generic-outer-product [m a spec]
    (let [scalar? (:scalar? spec) mul (:mul spec)]
      (if (scalar? m)
        (if (scalar? a)
          (mul m a)
          (gmp/generic-pre-scale a m spec))
        (cond
         (scalar? m)
         (gmp/generic-pre-scale a m spec)
         :else
         (mp/convert-to-nested-vectors
          (mp/element-map m (fn [v] (gmp/generic-pre-scale a v spec)))))))))

(extend-protocol gmp/PGenericSummable
  Object
  (generic-element-sum [a spec]
    (let [scalar? (:scalar? spec) add (:add spec)]
      (if (scalar? a)
        a
        (mp/element-reduce a add)))))

(extend-protocol gmp/PGenericSquare
  Object
   (generic-square [m spec] 
     (let [scalar? (:scalar? spec) mul (:mul spec)]
       (if (scalar? m)
         (mul m m)
         (gmp/generic-element-multiply m m spec)))))