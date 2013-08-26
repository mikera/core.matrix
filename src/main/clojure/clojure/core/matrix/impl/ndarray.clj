(ns clojure.core.matrix.impl.ndarray
  (:require [clojure.walk :as w])
  (:use clojure.core.matrix.utils)
  (:use clojure.core.matrix.impl.ndarray-magic)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.multimethods :as mm])
  (:refer-clojure :exclude [vector?]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; TODO: abstract Java-related stuff so it can be ported to JS
;; TODO: check explicit throwing of out-of-bounds exceptions everywhere

;; ## Intro
;;
;; This is an implementation of strided N-Dimensional array (or NDArray
;; for short). The underlying structure is similar to NumPy's [1] [2]
;; Default striding scheme is row-major, as in C. It can be changed
;; explicitly or by using some operations.
;;
;; TODO: elaborate on strides and stride-modifying operations

;; ## The structure
;;
;; This type is identical to NumPy's. Strides are stored explicitly;
;; this allows to perform some operations like transposition or
;; broadcasting to be done on "strides" field alone, avoiding touching
;; the data itself.
;; In future, other memory layouts can be considered, such as Morton order.
;; TODO: try Morton order
;; TODO: consider moving offset field to View instead of NDArray itself
;; I think that "offset" field should belong to a special "view" type,
;; because this way we can easily see when matrix refers to a bigger
;; memory chunk then it can.

;; TODO: doc this declare

;; ## Default striding schemes
;;
;; When we are using C-like striding (row-major ordering), the formula for
;; strides is as follows:
;; shape = (d_1, d_2, \dots, d_N)
;; strides = (s_1, s_2, \dots, s_N)
;; s_j = d_{j+1} d_{j+2} \dots d_N
;; (see [2])

(defn c-strides [shape]
  (conj (->> shape
             reverse
             (reductions *)
             reverse
             rest
             vec) 1))

;; We can easily check for correctness here using NumPy:
;; ```python
;; np.empty([4, 3, 2], dtype=np.int8, order="c").strides # (6, 2, 1)
;; ```
;; An actual test can be found in test_ndarray_implementation.clj.

;; When we are using Fortran-like striding (column-major ordering),
;; the formula for strides is different:
;; s_j = d_1 d_2 \dots d_{j-1}

(defn f-strides [shape]
  (->> shape
       (reductions *)
       butlast
       (cons 1)
       vec))

;; Again, it's easy to use NumPy to verify this implementation:
;; ```python
;; np.empty([4, 3, 2], dtype=np.int8, order="f").strides # (1, 4, 12)
;; ```
;; And again, an actual test can be found in test_ndarray_implementation.clj.


;; ## Helper functions
;;
;; In this section we define a couple of useful functions.
;; TODO: check if NDWrapper can reuse some of this; refactor to
;; utils.clj?
;;
;; First of them is a function to find an index inside of strided array.
;; General formula for finding an element of given index inside of a
;; strided array is
;; index = (n_1, n_2, \dots d_N)
;; offset = \sum_{i=0}^{N-1} s_i n_i
;; (see [1])
;; TODO: unroll this for common dimensions

(defmacro get-strided-idx
  "Returns an index inside of a strided array given a primitive long arrays
of indexes and strides"
  [idxs strides offset]
  `(+ (areduce ~idxs i# res# (int 0)
              (+ (* (aget ~idxs i#) (aget ~strides i#))
                 res#))
     ~offset))

(defmacro id-macro [x] x)

(init-magic
 {:object {:regname :ndarray
           :fn-suffix nil
           :typename 'NDArray
           :array-tag 'objects
           :array-cast 'object-array
           :type-cast 'identity
           :type-object java.lang.Object}
  :long {:regname :ndarray-long
         :fn-suffix 'long
         :typename 'NDArrayLong
         :array-tag 'longs
         :array-cast 'long-array
         :type-cast 'long
         :type-object Long/TYPE}
  :float {:regname :ndarray-float
          :fn-suffix 'float
          :typename 'NDArrayFloat
          :array-tag 'floats
          :array-cast 'float-array
          :type-cast 'float
          :type-object Float/TYPE}
  :double {:regname :ndarray-double
           :fn-suffix 'double
           :typename 'NDArrayDouble
           :array-tag 'doubles
           :array-cast 'double-array
           :type-cast 'double
           :type-object Double/TYPE}})

(with-magic
  [:long :float :double :object]
  (deftype typename#
      [^array-tag# data
       ^int ndims
       ^ints shape
       ^ints strides
       ^int offset]))

;; TODO: describe auto-declaring

;; ## Constructors
;;
;; Constructing of an empty NDArray with given shape is fairly easy.
;; Here, we provide an optional argument so user can choose to use
;; Fortran-like data layout.

(with-magic
  [:long :float :double :object]
  (defn empty-ndarray
    "Returns an empty NDArray of given shape"
    [shape & {:keys [order] :or {order :c}}]
    (let [shape (int-array shape)
          ndims (count shape)
          strides (case order
                    :c (int-array (c-strides shape))
                    :f (int-array (f-strides shape)))
          len (reduce * shape)
          data (array-cast# len)
          offset 0]
      (new typename# data ndims shape strides offset))))

(with-magic
  [:long :float :double]
  (defn empty-ndarray-zeroed
    "Returns an empty NDArray of given shape, guaranteed to be zeroed"
    [shape & {:keys [order] :or {order :c}}]
    (let [shape (int-array shape)
          ndims (count shape)
          strides (case order
                    :c (int-array (c-strides shape))
                    :f (int-array (f-strides shape)))
          len (reduce * shape)
          data (array-cast# len)
          offset 0]
      (new typename# data ndims shape strides offset))))

(with-magic
  [:object]
  (defn empty-ndarray-zeroed
    "Returns an empty NDArray of given shape, guaranteed to be zeroed"
    [shape & {:keys [order] :or {order :c}}]
    (let [shape (int-array shape)
          ndims (count shape)
          strides (case order
                    :c (int-array (c-strides shape))
                    :f (int-array (f-strides shape)))
          len (reduce * shape)
          data (array-cast# len)
          offset 0
          m (new typename# data ndims shape strides offset)]
      (java.util.Arrays/fill ^objects data
                             (cast java.lang.Object 0.0))
      m)))

;; Here we construct NDArray with given data. The caveat here is that we
;; can't really use this definition until we define an implementation for
;; protocol PIndexedSettingMutable because of mp/assign! use.

(with-magic
  [:long :float :double :object]
  (defn ndarray
    "Returns NDArray with given data, preserving shape of the data"
    [data]
    (let [mtx (empty-ndarray#t (mp/get-shape data))]
      (mp/assign! mtx data)
      mtx)))

;; TODO: this destructuring should really be a macro
;; TODO: doc
;; TODO: not sure that strides don't matter
;; TODO: check offset larger than array

(with-magic
  [:long :float :double :object]
  (defn arbitrary-slice
    [^typename# m dim idx]
    (iae-when-not (> (.ndims m) 0)
      (str "can't get slices on [" (.ndims m) "]-dimensional object"))
    (let [^array-tag# data (.data m)
          ndims (.ndims m)
          ^ints shape (.shape m)
          ^ints strides (.strides m)
          offset (.offset m)
          new-ndims (dec ndims)
          new-shape (abutnth dim shape)
          new-strides (abutnth dim strides)
          new-offset (+ offset (* idx (aget strides dim)))]
      (new typename# data new-ndims new-shape new-strides new-offset))))

(with-magic
  [:long :float :double :object]
  (defn row-major-slice
    [^typename# m idx]
    (arbitrary-slice#t m 0 idx)))

(with-magic
  [:long :float :double :object]
  (defn reshape-restride
    [^typename# m new-ndims ^ints new-shape ^ints new-strides new-offset]
    (let [^array-tag# data (.data m)
          new-ndims (int new-ndims)
          new-offset (int new-offset)]
      (new typename# data new-ndims new-shape new-strides new-offset))))

;; ## Seqable
;;
;; In general there is huge chunk of default-ish stuff that can be used here
;; (see Seqable implementation in wrappers that use PSliceSeq and
;; get-major-slice-seq, that in turn uses get-major-slice iteratively),
;; but it looks horribly inefficient, so let's build a lazy seq here.
;; NOTE: an actual implementations is now in the deftype itself, because
;; Seqable is not a real protocol
;; TODO: test for seq
;; TODO: test for .toString

(with-magic
  [:long :float :double :object]
  (defn row-major-seq [^typename# m]
    (iae-when-not (> (.ndims m) 0)
      (str "can't get slices on [" (.ndims m) "]-dimensional object"))
    (let [^ints shape (.shape m)]
      (map (partial row-major-slice#t m) (range (aget shape 0))))))

(with-magic
  [:long :float :double :object]
  (defn row-major-seq-no0d
    "like row-major-seq but drops NDArray's wrapping on 0d-slices"
    [^typename# m]
    (if (== (.ndims m) 1)
      (map mp/get-0d (row-major-seq#t m))
      (row-major-seq#t m))))

(extend-types-magic
  [:long :float :double :object]
  java.lang.Object
    (toString [m]
       (str (mp/persistent-vector-coerce m)))

  clojure.lang.Seqable
    (seq [m]
      (row-major-seq-no0d#t m))

  clojure.lang.Sequential

;; ## Mandatory protocols for all matrix implementations
;;
;; This bunch of protocols is mandatory for all core.matrix implementations.
;;
;; PImplementation protocol contains methods for providing implementation
;; metadata and matrix construction.

  mp/PImplementation
    (implementation-key [m] regname#)
    (meta-info [m]
      {:doc "An implementation of strided N-Dimensional array"})
    (new-vector [m length]
      (empty-ndarray-zeroed#t [length]))
    (new-matrix [m rows columns]
      (empty-ndarray-zeroed#t [rows columns]))
    (new-matrix-nd [m shape]
      (empty-ndarray#t shape))
    (construct-matrix [m data]
      (ndarray#t data))
    (supports-dimensionality? [m dims]
      true)

;; PDimensionInfo is for letting know core.matrix about dimensionality
;; of the matrix.

  mp/PDimensionInfo
    (get-shape [m] (vec shape))
    (is-vector? [m] (= 1 ndims))
    (is-scalar? [m] false)
    (dimensionality [m] ndims)
    (dimension-count [m x] (aget shape x))

;; PIndexedAccess protocol defines a bunch of functions to allow
;; (surprisingly) indexed access into matrix.
;; TODO: is it possible to avoid using this ugly type hints (also see [3])?

  mp/PIndexedAccess
    (get-1d [m x]
    ;; TODO: check if this check is really needed
      (iae-when-not (= 1 (.ndims m))
        "can't use get-1d on non-vector")
      (aget data (+ offset x)))
    (get-2d [m x y]
      (iae-when-not (= 2 (.ndims m))
        "can't use get-2d on non-matrix")
      (let [idx (+ offset
                   (+ (* (aget strides 0) (int x))
                      (* (aget strides 1) (int y))))]
        (aget data idx)))
    (get-nd [m indexes]
      (iae-when-not (= (count indexes) ndims)
        "index count should match dimensionality")
      (let [idxs (int-array indexes)
            idx (get-strided-idx idxs strides offset)]
        (aget data idx)))

;; PIndexedSetting is for non-mutative update of a matrix. Here we emulate
;; "non-mutative" setting by making a mutable copy and mutating it.

  mp/PIndexedSetting
    (set-1d [m row v]
      (let [m-new (mp/clone m)]
        (mp/set-1d! m-new row v)
        m-new))
    (set-2d [m row column v]
      (let [m-new (mp/clone m)]
        (mp/set-2d! m-new row column v)
        m-new))
    (set-nd [m indexes v]
      (let [m-new (mp/clone m)]
        (mp/set-nd! m-new indexes v)
        m-new))
    (is-mutable? [m] true)

;; ## Mandatory protocols for mutable matrix implementations
;;
;; In this section, protocols that help to work with mutable matrixes are
;; defined. It is worth noting that in the previous section, namely
;; PIndexedSetting protocol implementation, we used mutative operations,
;; therefore this section is required for previous section to work.
;;
;; PIndexedSettingMutable defines operations for matrix mutation at given
;; index.

  mp/PIndexedSettingMutable
    (set-1d! [m x v]
      (when-not (== 1 ndims)
        (throw (IllegalArgumentException. "can't use set-1d! on non-vector")))
      (aset data (+ offset x) (type-cast# v)))
    (set-2d! [m x y v]
      (when-not (== 2 ndims)
        (throw (IllegalArgumentException. "can't use set-2d! on non-matrix")))
      (let [idx (+ (* (aget strides 0) (int x))
                   (* (aget strides 1) (int y))
                   offset)]
        (aset data idx (type-cast# v))))
    (set-nd! [m indexes v]
      (when-not (= (count indexes) ndims)
        (throw (IllegalArgumentException.
                "index count should match dimensionality")))
      (let [idxs (int-array indexes)
            idx (get-strided-idx idxs strides offset)]
        (aset data idx (type-cast# v))))

;; PMatrixCloning requires only "clone" method, which is used to clone
;; mutable matrix. The mutation of clone must not affect the original.
;; TODO: an open question is whether we need to normalize memory layout here
;; (forcing data to conform C-order, for example) or not
;; TODO: move this constructor to constructors section

  mp/PMatrixCloning
    (clone [m]
      (let [data-new (aclone data)
            shape-new (aclone shape)
            strides-new (aclone strides)]
        (new typename# data-new ndims shape-new strides-new offset)))

;; ## Optional protocols
;;
;; Following protocols are implemented for performance or better behaviour.

  mp/PConversion
    (convert-to-nested-vectors [m]
      (case ndims
        0 (aget data offset)
        1 (let [n (aget shape 0)
                stride (aget strides 0)]
            (loop [idx (int offset)
                   cnt (int 0)
                   res []]
              (if (< cnt n)
                (recur (+ idx stride) (inc cnt) (conj res (aget data idx)))
                res)))
        ;; TODO: this can be done more efficiently
       (mapv mp/convert-to-nested-vectors
             (mp/get-major-slice-seq m))))

  mp/PTypeInfo
    (element-type [m] type-object#)

  mp/PMutableMatrixConstruction
    (mutable-matrix [m] (mp/clone m))

  mp/PZeroDimensionAccess
    (get-0d [m] (aget data offset))
    (set-0d! [m v] (aset data offset (type-cast# v)))

  mp/PSpecialisedConstructors
    (identity-matrix [m n]
      (let [^typename# new-m (empty-ndarray#t [n n])
            ^array-tag# new-m-data (.data new-m)]
        (when (= type-object# java.lang.Object)
          (c-for [i (int 0) (< i (* n n)) (inc i)]
            (aset new-m-data i (type-cast# 0))))
        (c-for [i (int 0) (< i n) (inc i)]
          (aset new-m-data (+ i (* i n)) (type-cast# 1)))
        new-m))
    (diagonal-matrix [m diag]
      (let [prim-diag (array-cast# diag)
            n (alength prim-diag)
            ^typename# new-m (empty-ndarray#t [n n])
            ^array-tag# new-m-data (.data new-m)]
        (when (= type-object# java.lang.Object)
          (c-for [i (int 0) (< i (* n n)) (inc i)]
            (aset new-m-data i (type-cast# 0))))
        (c-for [i (int 0) (< i n) (inc i)]
          (aset new-m-data (int (+ i (* i n)))
                (type-cast# (aget prim-diag i))))
        new-m))

  ;; mp/PCoercion
  ;;   (coerce-param [m param])
  ;; mp/PBroadcast
  ;;   (broadcast [m target-shape])
  ;; mp/PBroadcastLike
  ;;   (broadcast-like [m a])
  ;; mp/PReshaping
  ;;   (reshape [m shape])

  mp/PMatrixSlices
    (get-row [m i]
      (iae-when-not (== ndims 2)
        "get-row is applicable only for matrices")
      (row-major-slice#t m i))
    (get-column [m i]
      (iae-when-not (== ndims 2)
        "get-column is applicable only for matrices")
      (arbitrary-slice#t m 1 i))
    (get-major-slice [m i]
      (row-major-slice#t m i))
    (get-slice [m dimension i]
      (arbitrary-slice#t m dimension i))

  mp/PSubVector
    (subvector [m start length]
      (iae-when-not (== ndims 1)
        "subvector is applicable only for vectors")
      (let [new-shape (int-array 1 (int length))
            new-offset (+ offset (* (aget strides 0) start))]
        (reshape-restride#t m ndims new-shape strides new-offset)))

  mp/PSliceView
   (get-major-slice-view [m i] (row-major-slice#t m i))

  mp/PSliceSeq
    (get-major-slice-seq [m] (seq m))

  ;; mp/PSliceJoin
  ;;   (join [m a])

  ;; TODO: generalize for higher dimensions (think tensor trace)
  ;; TODO: make it work for rectangular matrices
  ;; TODO: clarify docstring about rectangulra matrices
  ;; TODO: clarify docstring about higher dimensions
  ;; TODO: rewrite + * as * inc
  mp/PMatrixSubComponents
    (main-diagonal [m]
      (iae-when-not (and (== ndims 2) (== (aget shape 0)
                                          (aget shape 1)))
        "main-diagonal is applicable only for square matrices")
      (let [new-ndims (int 1)
            new-shape (int-array 1 (aget shape 0))
            new-strides (int-array 1 (+ (* (aget shape 0)
                                           (aget strides 1))
                                        (aget strides 1)))]
        (reshape-restride#t m new-ndims new-shape new-strides offset)))

  ;; mp/PAssignment
  ;;   (assign! [m source])
  ;;   (assign-array! [m arr] [m arr start length])

  mp/PMutableFill
    (fill! [m v]
      (let [end (+ offset (areduce shape i s (int 1)
                                   (* s (aget shape i))))]
        (c-for [i offset (< i end) (inc i)]
          (aset data i (type-cast# v)))))

  mp/PElementCount
    (element-count [m]
      (areduce shape i s (int 1)
               (* s (aget shape i))))

  ;; mp/PTranspose
  ;;   (transpose [m]
  ;;     (iae-when-not (<= ndims 2)
  ;;       "transpose is applicable only for dimensions lower than 2")
  ;;     (let [new-shape (areverse shape)
  ;;           new-strides (areverse strides)]
  ;;       (reshape-restride#t m ndims new-shape new-strides offset)))

    )

;; TODO: optimize on smaller arrays
;; TODO: optimize vector-matrix and matrix-vector
;; TODO: optimize when second argument is different
;; TODO: replace messy striding code with macroses
;; TODO: replace stride multiplication with addition
;; (one can use explicit addition of stride instead of (inc i)
;; TODO: implement transposition of argument for faster access
;; TODO: be ready to normalize arguments if they are not in row-major
;; TODO: check bit.ly/16ECque for inspiration
;; TODO: for objects implement zeroing of target array
;; TODO: optimize element-multiply
;; For algorithms see [4]

(extend-types-magic
 [:double :float]
 mp/PMatrixMultiply
 (matrix-multiply [m a]
   (let [mdims ndims
         adims (int (mp/dimensionality a))]
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
      (let [a (if (= (type m) (type a)) a
                  (mp/coerce-param m a))]
        (let [mrows (aget shape (int 0))
              mcols (aget shape (int 1))
              arows (aget (int-array (.shape ^typename# a)) (int 0))
              acols (aget (int-array (.shape ^typename# a)) (int 1))
              ^typename# new-m (mp/new-matrix m mrows acols)
              ^ints nm-strides (.strides new-m)
              ^array-tag# nm-data (.data new-m)
              ^ints a-strides (int-array (.strides ^typename# a))
              ^array-tag# a-data (.data ^typename# a)
              a-offset (int (.offset ^typename# a))]
          (do
            (c-for [i (int 0) (< i mrows) (inc i)
                    k (int 0) (< k mcols) (inc k)]
              (let [t (aget data
                            (+ (+ (* (aget strides (int 0)) i)
                                  (* (aget strides (int 1)) k))
                               offset))]
                (c-for [j (int 0) (< j acols) (inc j)]
                  (let [nm-idx (+ (* (aget nm-strides (int 0)) i)
                                  (* (aget nm-strides (int 1)) j))]
                    (aset nm-data nm-idx
                          (+ (aget nm-data nm-idx)
                             (* t
                                (aget a-data
                                      (+ (+ (* (aget a-strides (int 0)) k)
                                            (* (aget a-strides (int 1)) j))
                                         a-offset)))))))))
            new-m))))))
 (element-multiply [m a]
   (if (number? a)
     (mp/scale m a)
     (let [[m a] (mp/broadcast-compatible m a)]
       (mp/element-map m clojure.core/* a)))))

(spit-code-magic)

;; ## Links
;; [1]: http://docs.scipy.org/doc/numpy/reference/arrays.ndarray.html
;; [2]: http://scipy-lectures.github.io/advanced/advanced_numpy/
;; [3]: http://clj-me.cgrand.net/2009/08/06/what-warn-on-reflection-doesnt-tell-you-about-arrays/
;; [4]: http://penguin.ewu.edu/~trolfe/MatMult/MatOpt.html