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
  Number
    (generic-element-multiply [m a spec]
      (if (number? a)
        ((:mul spec) m a)
        (mp/pre-scale a m)))
    (generic-matrix-multiply [m a spec]
      (cond
        (number? a) ((:mul spec) m a)
        (array? a) (mp/pre-scale a m)
        :else (error "Don't know how to multiply number with: " (class a))))
  Object
    (generic-matrix-multiply [m a spec]
      (let [mdims (long (mp/dimensionality m))
            adims (long (mp/dimensionality a))]
        (cond
         (== adims 0) (mp/scale m a)
         (and (== mdims 1) (== adims 2))
           (let [[arows acols] (mp/get-shape a)]
             (mp/reshape (mp/matrix-multiply (mp/reshape m [1 arows]) a)
                         [arows]))
         (and (== mdims 2) (== adims 1))
           (let [[mrows mcols] (mp/get-shape m)]
             (mp/reshape (mp/matrix-multiply m (mp/reshape a [mcols 1]))
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
                 (mp/set-2d! new-m i j ((:add spec) (mp/get-2d new-m i j)
                                          ((:mul spec) (mp/get-2d m i k)
                                             (mp/get-2d a k j)))))
               new-m)))))
    (generic-element-multiply [m a spec]
      (if (number? a)
        (mp/scale m a)
        (let [[m a] (mp/broadcast-compatible m a)]
          (mp/element-map m (:mul spec) a)))))

;; matrix multiply
(extend-protocol gmp/PGenericMatrixMultiplyMutable
  Number
    (generic-element-multiply! [m a spec]
      (error "Can't do mutable multiply on a scalar number"))
    (generic-matrix-multiply! [m a spec]
      (error "Can't do mutable multiply on a scalar number"))
  Object
    (generic-element-multiply! [m a spec]
      (mp/element-map! m (:mul spec) (mp/broadcast-like m a)))
    (generic-matrix-multiply! [m a spec]
      (mp/assign! m (mp/matrix-multiply m a))))

(extend-protocol gmp/PGenericMatrixDivide
  Number
    (generic-element-divide
      ([m spec] (/ m))
      ([m a spec] (mp/element-map a #(/ m %))))
  Object
    (generic-element-divide
      ([m spec] (mp/element-map m #(/ %)))
      ([m a spec]
        (let [[m a] (mp/broadcast-compatible m a)]
          (mp/element-map m #(/ %1 %2) a)))))

(extend-protocol gmp/PGenericMatrixDivideMutable
  Number
  (generic-element-divide!
    ([m spec] (error "Can't do mutable divide on a scalar number"))
    ([m a spec] (error "Can't do mutable divide on a scalar numer")))
  Object
  (generic-element-divide!
    ([m spec] (mp/element-map! m #(/ %)))
    ([m a spec]
       (let [[m a] (mp/broadcast-compatible m a)]
         (mp/element-map! m #(/ %1 %2) a)))))

;; matrix element summation
(extend-protocol gmp/PGenericSummable
  Number
    (generic-element-sum [a spec] a)
  Object
    (generic-element-sum [a spec]
      (mp/element-reduce a (:add spec))))

(extend-protocol gmp/PGenericElementMinMax
  Number
    (generic-element-min [m spec] m)
    (generic-element-max [m spec] m)
  Object
    (generic-element-min [m spec] 
      (mp/element-reduce m 
                       (fn [best v] (if (or (not best) (< v best)) v best)) 
                       nil))
    (generic-element-max [m spec] 
      (mp/element-reduce m 
                       (fn [best v] (if (or (not best) (> v best)) v best)) 
                       nil)))

;; add-product operations
(extend-protocol gmp/PGenericAddProduct
  Number
    (generic-add-product [m a b spec]
      ((:add spec) m ((:mul spec) a b)))
  Object
    (generic-add-product [m a b spec]
      (gmp/generic-matrix-add m (gmp/generic-element-multiply a b spec) spec)))

(extend-protocol gmp/PGenericAddProductMutable
  Number
    (generic-add-product! [m a b spec]
      (error "Numbers are not mutable"))
  Object
    (generic-add-product! [m a b spec]
      (mp/matrix-add! m (mp/element-multiply a b))))

(extend-protocol gmp/PGenericAddScaled
  Number
    (generic-add-scaled [m a factor spec]
      ((:add spec) m ((:mul spec) a factor)))
  Object
    (generic-add-scaled [m a factor spec]
      (mp/matrix-add m (mp/scale a factor))))

(extend-protocol gmp/PGenericAddScaledMutable
  Number
    (generic-add-scaled! [m a factor spec]
      (error "Numbers are not mutable"))
  Object
    (generic-add-scaled! [m a factor spec]
      (mp/matrix-add! m (mp/scale a factor))))

(extend-protocol gmp/PGenericAddScaledProduct
  Number
    (generic-add-scaled-product [m a b factor spec]
      ((:add spec) m ((:mul spec) a b factor)))
  Object
    (generic-add-scaled-product [m a b factor spec]
      (mp/matrix-add m (mp/scale (mp/element-multiply a b) factor))))

(extend-protocol gmp/PGenericAddScaledProductMutable
  Number
    (generic-add-scaled-product! [m a b factor spec]
      (error "Numbers are not mutable"))
  Object
    (generic-add-scaled-product! [m a b factor spec]
      (mp/matrix-add! m (mp/scale (mp/element-multiply a b) factor))))


;; matrix scaling
(extend-protocol gmp/PGenericMatrixScaling
  Number
    (generic-scale [m a spec]
      (if ((:scalar? spec) a)
        ((:mul spec) m a)
        (mp/pre-scale a m)))
    (generic-pre-scale [m a spec]
      (if ((:scalar? spec) a)
        ((:mul spec) a m)
        (mp/scale a m)))
  Object
    (generic-scale [m a spec]
      (mp/element-map m #((:mul spec) % a)))
    (generic-pre-scale [m a spec]
      (mp/element-map m (partial (:mul spec) a))))

(extend-protocol gmp/PGenericMatrixMutableScaling
  Number
    (generic-scale! [m a spec]
      (error "Can't scale! a numeric value: " m))
    (generic-pre-scale! [m a spec]
      (error "Can't pre-scale! a numeric value: " m))
  Object
    (generic-scale! [m a spec]
      (mp/element-map! m #((:mul spec) % a))
      m)
    (generic-pre-scale! [m a spec]
      (mp/element-map! m (partial (:mul spec) a))
      m))

(extend-protocol gmp/PGenericMatrixAdd
  ;; matrix add for scalars
  Number
    (generic-matrix-add [m a spec]
      (if ((:scalar? spec) a) ((:add spec) m a)
        (mp/matrix-add a m)))
    (generic-matrix-sub [m a spec]
      (if ((:scalar? spec) a) ((:sub spec) m a)
        (mp/negate (mp/matrix-sub a m))))
  ;; default impelementation - assume we can use emap?
  Object
    (generic-matrix-add [m a spec]
      (let [[m a] (mp/broadcast-compatible m a)]
        (mp/element-map m (:add spec) a)))
    (generic-matrix-sub [m a spec]
      (let [[m a] (mp/broadcast-compatible m a)]
        (mp/element-map m (:sub spec) a))))

(extend-protocol gmp/PGenericMatrixAddMutable
  ;; matrix add for scalars
  Number
    (generic-matrix-add! [m a spec]
      (error "Can't do mutable add! on a scalar number"))
    (generic-matrix-sub! [m a spec]
      (error "Can't do mutable sub! on a scalar number"))
  ;; default impelementation - assume we can use emap?
  Object
    (generic-matrix-add! [m a spec]
      (mp/element-map! m (:add spec) a))
    (generic-matrix-sub! [m a spec]
      (mp/element-map! m (:sub spec) a)))

(extend-protocol gmp/PGenericNegation
  nil
    (generic-negate [m spec]
      (error "Can't negate nil!"))
  Number
    (generic-negate [m spec]
      ((:sub spec) m))
  Object
    (generic-negate [m spec]
      (mp/scale m -1)))

;; equality checking
(extend-protocol gmp/PGenericMatrixEquality
  nil
    (generic-matrix-equals [a b spec]
      (error "nil is not a valid numerical value in equality testing"))
  Number
    (generic-matrix-equals [a b spec]
      (cond
        ((:scalar? spec) b) ((:= spec) a b)
        (== 0 (mp/dimensionality b)) ((:= spec) a (scalar-coerce b))
        :else false))
  Object
    (generic-matrix-equals [a b spec]
      (cond
        (identical? a b) true
        (mp/same-shape? a b)
          (if (== 0 (mp/dimensionality a))
            ((:= spec) (mp/get-0d a) (scalar-coerce b))
            (not (some false? (map (:= spec) (mp/element-seq a) (mp/element-seq b)))))
        :else false)))

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
      (and
        (mp/same-shape? a b)
        (every? true? (map = (mp/element-seq a) (mp/element-seq b))))))

(defmacro eps== [a b eps spec]
  `((:<= ~spec) ((:abs ~spec) ((:sub ~spec) (double ~a) (double ~b))) (double ~eps) ))

;; equality checking
(extend-protocol gmp/PGenericMatrixEqualityEpsilon
  nil
    (generic-matrix-equals-epsilon [a b eps spec]
      (error "nil is not a valid numerical value in equality testing"))
  Number
    (generic-matrix-equals-epsilon [a b eps spec]
      (cond
        ((:scalar? spec) b) (eps== a b eps spec)
        (== 0 (mp/dimensionality b)) (eps== a (mp/get-0d b) eps spec)
        :else false))
  Object
    (generic-matrix-equals-epsilon [a b eps spec]
      (cond
        (identical? a b) true
        (mp/same-shape? a b)
          (let [eps (double eps)]
            (every? #((:<= spec) ((:abs spec) (double %)) eps) (map (:sub spec) (mp/element-seq a) (mp/element-seq b))))
        :else false)))



(extend-protocol gmp/PGenericVectorOps
  Number
    (generic-vector-dot [a b spec] (gmp/generic-pre-scale b a spec))
    (generic-length [a spec] (double a))
    (generic-length-squared [a spec] (Math/sqrt (double a)))
    (generic-normalise [a spec]
      (let [a (double a)]
        (cond
          (> a 0.0) 1.0
          (< a 0.0) -1.0
          :else 0.0)))
  Object
    (generic-vector-dot [a b spec]
      (gmp/generic-element-sum (gmp/generic-element-multiply a b spec) spec))
    (generic-length [a spec]
      ((:sqrt spec) (gmp/generic-length-squared a spec)))
    (generic-length-squared [a spec]
      (mp/element-reduce a (fn [r x] ((:add spec) r ((:mul spec) x x))) 0))
    (generic-normalise [a spec]
      (mp/scale a ((:div spec) (:one spec) ((:sqrt spec) (gmp/generic-length-squared a spec))))))

(extend-protocol gmp/PGenericVectorDistance
  Number
    (generic-distance [a b spec] ((:abs spec) (- b a)))
  Object
    (generic-distance [a b spec] (gmp/generic-length (gmp/generic-matrix-sub a b spec) spec)))


(extend-protocol gmp/PGenericSummable
  Number
    (generic-element-sum [a spec] a)
  Object
    (generic-element-sum [a spec]
      (mp/element-reduce a (:add spec))))

(extend-protocol gmp/PGenericSquare
  Number
   (generic-square [m spec] ((:mul spec) m m))
  Object
   (generic-square [m spec] (gmp/generic-element-multiply m m spec)))