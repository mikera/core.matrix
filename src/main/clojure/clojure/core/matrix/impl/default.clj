(ns clojure.core.matrix.impl.default
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.impl.wrappers :as wrap])
  (:require [clojure.core.matrix.multimethods :as mm])
  (:require [clojure.core.matrix.impl.mathsops :as mops]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; ============================================================
;; Utility functions for default implementations

(defn array?
  "Returns true if the parameter is an N-dimensional array of any type"
  ([m]
    (satisfies? mp/PImplementation m)))

(defn square?
  "Returns true if matrix is square (2D with same number of rows and columns)"
  ([m]
    (and
      (== 2 (mp/dimensionality m))
      (== (mp/dimension-count m 0) (mp/dimension-count m 1)))))

(defn element-count
  "Returns the total count of elements in an array"
  ([m]
    (cond
      (array? m) (reduce * 1 (mp/get-shape m))
      :else (count m))))

;; ============================================================
;; Default implementations
;; - default behaviour for java.lang.Number scalars
;; - for stuff we don't recognise (java.lang.Object) we should try to
;;   implement in terms of simpler operations, on assumption that
;;   we have fallen through to the default implementation

;; default implementation for matrix ops

(extend-protocol mp/PIndexedAccess
  java.util.List
    (get-1d [m x]
      (.get m (int x)))
    (get-2d [m x y]
      (mp/get-1d (.get m (int x)) y))
    (get-nd [m indexes]
      (if-let [s (seq indexes)]
        (mp/get-nd (.get m (int (first s))) (next s))
        m))
  java.lang.Number
    (get-1d [m x]
      (error "Can't do 1D get on a scalar number"))
    (get-2d [m x y]
      (error "Can't do 2D get on a scalar number"))
    (get-nd [m indexes]
      (if-let [s (seq indexes)]
        (error "Can't do ND get on a scalar number with indexes: " s)
        m))
  java.lang.Object
    (get-1d [m x] (mp/get-nd m [x]))
    (get-2d [m x y] (mp/get-nd m [x y]))
    (get-nd [m indexes]
      (if (seq indexes)
        (error "Indexed get failed, not defined for:" (class m))
        (if (mp/is-scalar? m) m (mp/get-0d m)))))



(extend-protocol mp/PZeroDimensionAccess
  nil
    (get-0d [m]
      nil)
    (set-0d! [m value]
      (error "Can't set the value of nil!"))
  java.lang.Number
    (get-0d [m]
      m)
    (set-0d! [m value]
      (error "Can't set a scalar number!"))
  java.lang.Object
    (get-0d [m]
      (if (mp/is-scalar? m) m (mp/get-nd m [])))
    (set-0d! [m value]
      (mp/set-nd! m [] value)))

(extend-protocol mp/PIndexedSetting
  java.lang.Object
    (set-1d [m row v]
      (let [m (mp/clone m)]
        (mp/set-1d! m row v)
        m))
    (set-2d [m row column v]
      (let [m (mp/clone m)]
        (mp/set-2d! m row column v)
        m))
    (set-nd [m indexes v]
      (let [m (mp/clone m)]
        (mp/set-nd! m indexes v)
        m))
    (is-mutable? [m]
      true))

(extend-protocol mp/PVectorOps
  java.lang.Number
    (vector-dot [a b] (* a b))
    (length [a] (double a))
    (length-squared [a] (Math/sqrt (double a)))
    (normalise [a]
      (let [a (double a)]
        (cond
          (> a 0.0) 1.0
          (< a 0.0) -1.0
          :else 0.0)))
  java.lang.Object
    (vector-dot [a b]
      (reduce + 0 (mp/element-seq (mp/element-multiply a b))))
    (length [a]
      (Math/sqrt (double (mp/length-squared a))))
    (length-squared [a]
      (mp/element-reduce a (fn [r x] (+ r (* x x))) 0))
    (normalise [a]
      (mp/scale a (/ 1.0 (Math/sqrt (double (mp/length-squared a)))))))

(extend-protocol mp/PVectorDistance
  java.lang.Number
    (distance [a b] (Math/abs (double (- b a))))
  java.lang.Object
    (distance [a b] (double (mp/length (mp/matrix-sub a b)))))

(extend-protocol mp/PVectorCross
  java.lang.Object
    (cross-product [a b]
      (let [x1 (double (mp/get-1d a 0))
            y1 (double (mp/get-1d a 1))
            z1 (double (mp/get-1d a 2))
            x2 (double (mp/get-1d b 0))
            y2 (double (mp/get-1d b 1))
            z2 (double (mp/get-1d b 2))]
        (mp/construct-matrix a [(- (* y1 z2) (* z1 y2))
                                (- (* z1 x2) (* x1 z2))
                                (- (* x1 y2) (* y1 x2))])))
    (cross-product! [a b]
      (let [x1 (double (mp/get-1d a 0))
            y1 (double (mp/get-1d a 1))
            z1 (double (mp/get-1d a 2))
            x2 (double (mp/get-1d b 0))
            y2 (double (mp/get-1d b 1))
            z2 (double (mp/get-1d b 2))]
        (mp/set-1d! a 0 (- (* y1 z2) (* z1 y2)))
        (mp/set-1d! a 1 (- (* z1 x2) (* x1 z2)))
        (mp/set-1d! a 2 (- (* x1 y2) (* y1 x2)))
        a))) 

(extend-protocol mp/PMutableVectorOps
  java.lang.Object
    (normalise! [a]
      (mp/scale! a (/ 1.0 (Math/sqrt (double (mp/length-squared a)))))))

(extend-protocol mp/PAssignment
  java.lang.Object
    (assign! [m x]
      (let [dims (mp/dimensionality m)]
        (cond
          (== 1 dims)
	          (dotimes [i (mp/dimension-count m 0)]
	              (mp/set-1d! m i (mp/get-1d x i))) 
          (== 0 dims) (mp/set-0d! m (mp/get-0d x))
	        (array? m)
	          (doall (map (fn [a b] (mp/assign! a b))
	                      (mp/get-major-slice-seq m)
	                      (mp/get-major-slice-seq x)))
	        :else
	          (error "Can't assign to a non-array object: " (class m)))))
    (assign-array!
      ([m arr]
	      (let [alen (long (count arr))]
	        (if (mp/is-vector? m)
	          (dotimes [i alen]
	            (mp/set-1d! m i (nth arr i)))
	          (mp/assign-array! m arr 0 alen))))
      ([m arr start length]
	      (let [length (long length)
              start (long start)]
         (if (mp/is-vector? m)
	          (dotimes [i length]
	            (mp/set-1d! m i (nth arr (+ start i))))
	          (let [ss (seq (mp/get-major-slice-seq m))
	                skip (long (if ss (element-count (first (mp/get-major-slice-seq m))) 0))]
	            (doseq-indexed [s ss i]
	              (mp/assign-array! s arr (+ start (* skip i)) skip))))))))

(extend-protocol mp/PMatrixCloning
	  java.lang.Cloneable
	    (clone [m]
	      (.invoke ^java.lang.reflect.Method (.getDeclaredMethod (class m) "clone" nil) m nil))
	  java.lang.Object
	    (clone [m]
	      (mp/coerce-param m (mp/coerce-param [] m))))

(extend-protocol mp/PDimensionInfo
  nil
    (dimensionality [m] 0)
    (is-scalar? [m] true)
    (is-vector? [m] false)
    (get-shape [m] [])
    (dimension-count [m i] (error "cannot get dimension count from nil"))
  java.lang.Number
    (dimensionality [m] 0)
    (is-scalar? [m] true)
    (is-vector? [m] false)
    (get-shape [m] [])
    (dimension-count [m i] (error "java.lang.Number has zero dimensionality, cannot get dimension count"))
  java.lang.Object
    (dimensionality [m] 0)
    (is-vector? [m] (== 1 (mp/dimensionality m)))
    (is-scalar? [m] true) ;; assume objects are scalars unless told otherwise
    (get-shape [m] (for [i (range (mp/dimensionality m))] (mp/dimension-count m i)))
    (dimension-count [m i] (error "Can't determine count of dimension " i " on Object: " (class m))))


;; generic versions of matrix ops
(extend-protocol mp/PMatrixOps
  java.lang.Object
    (trace [m]
      (when-not (== 2 (mp/dimensionality m)) (error "Trace requires a 2D matrix"))
      (let [rc (mp/dimension-count m 0)
            cc (mp/dimension-count m 1)
            dims (long rc)]
        (when-not (== rc cc) (error "Can't compute trace of non-square matrix"))
        (loop [i 0 res 0.0]
          (if (>= i dims)
            res
            (recur (inc i) (+ res (double (mp/get-2d m i i))))))))
    (negate [m]
      (mp/scale m -1.0))
    (length-squared [m]
      (mp/element-reduce #(+ %1 (* %2 *2)) 0.0 m))
    (length [m]
      (Math/sqrt (mp/length-squared m)))
    (transpose [m]
      (case (long (mp/dimensionality m))
        0 m
        1 m
        2 (mp/coerce-param m (apply mapv vector (map 
                                                  #(mp/coerce-param [] %) 
                                                  (mp/get-major-slice-seq m))))
        (mp/coerce-param m 
          (let [ss (map mp/transpose (mp/get-major-slice-seq m))] 
            ;; note than function must come second for mp/element-map   
            (apply mp/element-map (first ss) vector (next ss)))))))

;; matrix multiply
(extend-protocol mp/PMatrixMultiply
  java.lang.Number
    (element-multiply [m a]
      (clojure.core/* m a))
    (matrix-multiply [m a]
      (cond
        (number? a) (* m a)
        (array? a) (mp/pre-scale a m)
        :else (error "Don't know how to multiply number with: " (class a))))
  java.lang.Object
    (matrix-multiply [m a]
      (mp/coerce-param m (mp/matrix-multiply 
                           (mp/coerce-param [] m) 
                           (mp/coerce-param [] a))))
    (element-multiply [m a]
      (mp/element-map m clojure.core/* a)))

;; matrix multiply
(extend-protocol mp/PMatrixMultiplyMutable
  java.lang.Number
    (element-multiply! [m a]
      (error "Can't do mutable multiply on a scalar number"))
    (matrix-multiply! [m a]
      (error "Can't do mutable multiply on a scalar number"))
  java.lang.Object
    (element-multiply! [m a]
      (mp/element-map! m * a))
    (matrix-multiply! [m a]
      (mp/assign! m (mp/matrix-multiply m a))))

;; matrix element summation
(extend-protocol mp/PSummable
  java.lang.Number
    (element-sum [a] a)
  java.lang.Object
    (element-sum [a]
      (mp/element-reduce a +)))

;; type of matrix element
;; the default is to assume any type is possible
(extend-protocol mp/PTypeInfo
  java.lang.Object
    (element-type [a] 
      java.lang.Object))

;; general transformation of a vector
(extend-protocol mp/PVectorTransform
  clojure.lang.IFn
    (vector-transform [m a]
      (m a))
    (vector-transform! [m a]
      (mp/assign! a (m a)))
  java.lang.Object
    (vector-transform [m a]
      (cond
        (== 2 (mp/dimensionality m)) (mp/matrix-multiply m a)
        :else (error "Don't know how to transform using: " (class m))))
    (vector-transform! [m a]
      (mp/assign! a (mp/vector-transform m a))))

;; matrix scaling
(extend-protocol mp/PMatrixScaling
  java.lang.Number
    (scale [m a]
      (if (number? a)
        (* m a)
        (mp/pre-scale a m)))
    (pre-scale [m a]
      (if (number? a)
        (* a m)
        (mp/scale a m)))
  java.lang.Object
    (scale [m a]
      (mp/element-map m #(* % a)))
    (pre-scale [m a]
      (mp/element-map m (partial * a))))

(extend-protocol mp/PMatrixMutableScaling
  java.lang.Number
    (scale! [m a]
      (error "Can't scale! a numeric value: " m))
    (pre-scale! [m a]
      (error "Can't pre-scale! a numeric value: " m))
  java.lang.Object
    (scale! [m a]
      (mp/element-map! m #(* % a)))
    (pre-scale! [m a]
      (mp/element-map! m (partial * a))))

(extend-protocol mp/PMatrixAdd
  ;; matrix add for scalars
  java.lang.Number
    (matrix-add [m a]
      (if (number? a) (+ m a) (error "Can't add scalar number to a matrix")))
    (matrix-sub [m a]
      (if (number? a) (- m a) (error "Can't a matrix from a scalar number")))
  ;; default impelementation - assume we can use emap?
  java.lang.Object
    (matrix-add [m a]
      (mp/element-map m clojure.core/+ a))
    (matrix-sub [m a]
      (mp/element-map m clojure.core/- a)))

(extend-protocol mp/PMatrixAddMutable
  ;; matrix add for scalars
  java.lang.Number
    (matrix-add! [m a]
      (error "Can't do mutable add! on a scalar number"))
    (matrix-sub! [m a]
      (error "Can't do mutable sub! on a scalar number"))
  ;; default impelementation - assume we can use emap?
  java.lang.Object
    (matrix-add! [m a]
      (mp/element-map! m clojure.core/+ a))
    (matrix-sub! [m a]
      (mp/element-map! m clojure.core/- a)))

;; equality checking
(extend-protocol mp/PMatrixEquality
  java.lang.Number
    (matrix-equals [a b]
      (== a (if (number? b) b (mp/get-0d b))))
  java.lang.Object
    (matrix-equals [a b]
      (not (some false? (map == (mp/element-seq a) (mp/element-seq b))))))

(extend-protocol mp/PDoubleArrayOutput
  java.lang.Number
    (to-double-array [m] (aset (double-array 1) 0 (double m)))
    (as-double-array [m] nil)
  java.lang.Object
    (to-double-array [m]
      (double-array (mp/element-seq m)))
    (as-double-array [m] nil)) 

;; functional operations
(extend-protocol mp/PFunctionalOperations
  java.lang.Number
    (element-seq [m]
      (list m))
    (element-map
      ([m f]
        (f m))
      ([m f a]
        (f m a))
      ([m f a more]
        (apply f m a more)))
    (element-map!
      ([m f]
        (error "java.lang.Number instance is not mutable!"))
      ([m f a]
        (error "java.lang.Number instance is not mutable!"))
      ([m f a more]
        (error "java.lang.Number instance is not mutable!")))
    (element-reduce
      ([m f]
        m)
      ([m f init]
        (f init m)))
  java.lang.Object
    (element-seq [m]
      (let [dims (mp/dimensionality m)]
        (cond
          (== 0 dims) 
            (if (mp/is-scalar? m) (list m) (list (mp/get-0d m))) 
          (== 1 dims) 
            (map #(mp/get-1d m %) (range (mp/dimension-count m 0))) 
          (array? m) 
            (mapcat mp/element-seq (mp/get-major-slice-seq m))
          :else (error "Don't know how to create element-seq from: " m))))
    (element-map
      ([m f]
        (mp/coerce-param m (mp/element-map (mp/convert-to-nested-vectors m) f)))
      ([m f a]
        (mp/coerce-param m (mp/element-map (mp/convert-to-nested-vectors m) f a)))
      ([m f a more]
        (mp/coerce-param m (mp/element-map (mp/convert-to-nested-vectors m) f a more))))
    (element-map!
      ([m f]
        (mp/assign! m (mp/element-map m f)))
      ([m f a]
        (mp/assign! m (mp/element-map m f a)))
      ([m f a more]
        (mp/assign! m (mp/element-map m f a more))))
    (element-reduce
      ([m f]
        (reduce f (mp/element-seq m)))
      ([m f init]
        (reduce f init (mp/element-seq m))))
  nil
    (element-seq [m] '(nil))
    (element-map
      ([m f] (f nil))
      ([m f a] (f nil a))
      ([m f a more] (apply f nil a more)))
    (element-map!
      ([m f] (error "Can't do element-map! on nil"))
      ([m f a] (error "Can't do element-map! on nil"))
      ([m f a more] (error "Can't do element-map! on nil")))
    (element-reduce
      ([m f] (f nil))
      ([m f init] (f init nil))))

(extend-protocol mp/PMatrixSlices
  java.lang.Object
    (get-row [m i]
      (mp/get-major-slice m i))
    (get-column [m i]
      (mp/get-slice m 1 i))
    (get-major-slice [m i]
      (clojure.core.matrix.impl.wrappers/wrap-slice m i))
    (get-slice [m dimension i]
      (mp/get-slice (mp/coerce-param [] m) dimension i))) 

(extend-protocol mp/PSliceView
  java.lang.Object
    ;; default implementation uses a lightweight wrapper object
    (get-major-slice-view [m i] (clojure.core.matrix.impl.wrappers/wrap-slice m i)))

(extend-protocol mp/PSliceSeq
  java.lang.Object
    (get-major-slice-seq [m] 
      (let [dims (mp/dimensionality m)] 
        (cond 
          (<= dims 0)
            (error "Can't get slices on [" dims "]-dimensional object: " m)
          :else (map #(mp/get-major-slice m %) (range (mp/dimension-count m 0)))))))

(extend-protocol mp/PSubVector
  java.lang.Object
    (subvector [m start length]
      (mp/subvector (wrap/wrap-nd m) start length))) 

(extend-protocol mp/PSubMatrix
  java.lang.Number
    (submatrix [m index-ranges]
      (if (seq index-ranges)
        (error "Can't take partial submatrix of a scalr number")
        m))
  java.lang.Object
    (submatrix [m index-ranges]
      (clojure.core.matrix.impl.wrappers/wrap-submatrix m index-ranges))) 

(extend-protocol mp/PBroadcast
  java.lang.Object
    (broadcast [m shape] 
      (clojure.core.matrix.impl.wrappers/wrap-broadcast m shape)))


;; attempt conversion to nested vectors
(extend-protocol mp/PConversion
  nil
    (convert-to-nested-vectors [m]
      nil)
  java.lang.Number
    (convert-to-nested-vectors [m]
      ;; we accept a scalar as a "nested vector" for these purposes?
      m)
  java.lang.Object
    (convert-to-nested-vectors [m]
      (let [dims (mp/dimensionality m)]
        (cond
          (<= dims 0)
	          (mp/get-0d m)
	        (== 1 dims)
	          (mapv #(mp/get-1d m %) (range (mp/dimension-count m 0)))
	        (array? m)
	          (mapv mp/convert-to-nested-vectors (mp/get-major-slice-seq m))
	        (sequential? m)
	          (mapv mp/convert-to-nested-vectors m)
	        (seq? m)
	          (mapv mp/convert-to-nested-vectors m)
	        :default
	          (error "Can't work out how to convert to nested vectors: " (class m) " = " m)))))

(extend-protocol mp/PReshaping
  java.lang.Number
    (reshape [m shape]
      (case (long (reduce * 1 (seq shape)))
        0 (mp/broadcast m shape)
        1 (mp/broadcast m shape)
        (error "Can reshape a scalar value to shape: " (vec shape))))
  java.lang.Object
    (reshape [m shape]
      (let [partition-shape (fn partition-shape [es shape]
                              (if-let [s (seq shape)]
                                (let [ns (next s)
                                      plen (reduce * 1 ns)]
                                  (map #(partition-shape % ns) (partition plen es)))
                                (first es)))]
        (if-let [shape (seq shape)]
          (let [fs (long (first shape))
                parts (partition-shape (mp/element-seq m) shape)] 
            (when-not (<= fs (count parts))
              (error "Reshape not possible: insufficient elements for shape: " shape " have: " (seq parts)))
            (mp/construct-matrix m (take fs parts)))
          (first (mp/element-seq m))))))

(extend-protocol mp/PCoercion
  java.lang.Object
    (coerce-param [m param]
      (mp/construct-matrix m (mp/convert-to-nested-vectors param))))

;; define standard Java maths functions for numbers
(eval
  `(extend-protocol mp/PMathsFunctions
     java.lang.Number
       ~@(map (fn [[name func]]
                `(~name [~'m] (double (~func (double ~'m)))))
              mops/maths-ops)
     java.lang.Object
       ~@(map (fn [[name func]]
                `(~name [~'m] (mp/element-map ~'m #(double (~func (double %))))))
              mops/maths-ops)
       ~@(map (fn [[name func]]
                `(~(symbol (str name "!")) [~'m] (mp/element-map! ~'m #(double (~func (double %))))))
              mops/maths-ops)))

(extend-protocol mp/PMatrixSubComponents
  java.lang.Object
    (main-diagonal [m]
      (let [sh (mp/get-shape m)
            rank (count sh)
            dims (first sh)]
        (if-not (reduce = sh) (error "Not a square array!"))
        (mp/construct-matrix m (for [i (range dims)] (mp/get-nd m (repeat rank i)))))))

(extend-protocol mp/PSpecialisedConstructors
  java.lang.Object
    (identity-matrix [m dims]
      (mp/diagonal-matrix m (repeat dims 1.0)))
    (diagonal-matrix [m diagonal-values]
      (let [dims (count diagonal-values)
            diagonal-values (mp/coerce-param [] diagonal-values)
            zs (vec (repeat dims 0.0))
            dm (vec (for [i (range dims)]
                 (assoc zs i (nth diagonal-values i))))]
        (mp/coerce-param m dm))))

;; =======================================================
;; default multimethod implementations

(defmethod mm/mul :default [x y]
  (error "Don't know how to multiply " (class x) " with " (class y)))
