(ns clojure.core.matrix.impl.ndarray
  (:use clojure.test)
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.multimethods :as mm])
  (:refer-clojure :exclude [vector?]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

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
;; TODO: consider adding "offset" field (can be usable in slicing?)

(deftype NDArray
    [^objects data
     ^long ndims
     ^longs shape
     ^longs strides])

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
;;
;; ## Constructors
;;
;; Constructing of an empty NDArray with given shape is fairly easy.
;; Here, we provide an optional argument so user can choose to use
;; Fortran-like data layout.

(defn empty-ndarray
  "Returns an empty NDArray of given shape"
  [shape & {:keys [order] :or {order :c}}]
  (let [shape (long-array shape)
        ndims (count shape)
        strides (case order
                  :c (long-array (c-strides shape))
                  :f (long-array (f-strides shape)))
        len (reduce * shape)
        data (object-array len)]
    (NDArray. data ndims shape strides)))

;; Here we construct NDArray with given data. The caveat here is that we
;; can't really use this definition until we define an implementation for
;; protocol PIndexedSettingMutable because of mp/assign! use.

(defn ndarray
  "Returns NDArray with given data, preserving shape of the data"
  [data]
  (let [shape (long-array (mp/get-shape data))
        empty (empty-ndarray shape)]
    (mp/assign! empty data)))

;; ## Helper functions
;;
;; In this section we define a couple of useful functions.
;;
;; First of them is a function to find an index inside of strided array.
;; General formula for finding an element of given index inside of a
;; strided array is
;; index = (n_1, n_2, \dots d_N)
;; offset = \sum_{i=0}^{N-1} s_i n_i
;; (see [1])

(defn get-strided-idx
  "Returns an index inside of a strided array given a primitive long arrays
of indexes and strides"
  ^long [^longs idxs ^longs strides]
  (areduce idxs i res (long 0)
           (+ (* (aget idxs i) (aget strides i))
              res)))

;; ## Mandatory protocols for all matrix implementations
;;
;; This bunch of protocols is mandatory for all core.matrix implementations.
;;
;; PImplementation protocol contains methods for providing implementation
;; metadata and matrix construction.
;; TODO: check if defining this implementations in deftype's body will
;; inflict sensible performance benefit.

(extend-type NDArray
  mp/PImplementation
  (implementation-key [m] :ndarray)
  (new-vector [m length]
    (empty-ndarray [length]))
  (new-matrix [m rows columns]
    (empty-ndarray [rows columns]))
  (new-matrix-nd [m shape]
    (empty-ndarray shape))
  (construct-matrix [m data]
    (ndarray data))
  (supports-dimensionality? [m dims]
    true))

;; PDimensionInfo is for letting know core.matrix about dimensionality
;; of the matrix.

(extend-type NDArray
  mp/PDimensionInfo
  (get-shape [m]
    (vec (.shape m))) ;; TODO: check if we really need primitive here
  (is-vector? [m]
    (= 1 (.ndims m)))
  (is-scalar? [m]
    false)
  (dimensionality [m]
    (.ndims m))
  (dimension-count [m x]
    (aget (longs (.shape m)) x)))

;; PIndexedAccess protocol defines a bunch of functions to allow
;; (surprisingly) indexed access into matrix.
;; TODO: is it possible to avoid using this ugly type hints (also see [3])?

(extend-type NDArray
  mp/PIndexedAccess
  (get-1d [m x]
    ;; TODO: check if this check is really needed
    (when-not (= 1 (.ndims m))
      (throw (IllegalArgumentException. "can't use get-1d on non-vector")))
    (let [#^"[Ljava.lang.Object;" data (.data m)]
      (aget data x)))
  (get-2d [m x y]
    (when-not (= 2 (.ndims m))
      (throw (IllegalArgumentException. "can't use get-2d on non-matrix")))
    (let [^longs strides (.strides m)
          #^"[Ljava.lang.Object;" data (.data m)
          idx (+ (* (aget strides 0) (long x))
                 (* (aget strides 1) (long y)))]
      (aget data idx)))
  (get-nd [m indexes]
    (when-not (= (count indexes) (.ndims m))
      (throw (IllegalArgumentException.
              "index count should match dimensionality")))
    (let [idxs (long-array indexes)
          strides (.strides m)
          #^"[Ljava.lang.Object;" data (.data m)
          idx (get-strided-idx idxs strides)]
      (aget data idx))))

;; PIndexedSetting is for non-mutative update of a matrix. Here we emulate
;; "non-mutative" setting by making a mutable copy and mutating it.

(extend-type NDArray
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
  (is-mutable? [m]
    true))

;; ## Mandatory protocols for mutable matrix implementations
;;
;; In this section, protocols that help to work with mutable matrixes are
;; defined. It is worth noting that in the previous section, namely
;; PIndexedSetting protocol implementation, we used mutative operations,
;; therefore this section is required for previous section to work.
;;
;; PIndexedSettingMutable defines operations for matrix mutation at given
;; index.

(extend-type NDArray
  mp/PIndexedSettingMutable
  (set-1d! [m x v]
    (when-not (= 1 (.ndims m))
      (throw (IllegalArgumentException. "can't use set-1d! on non-vector")))
    (let [#^"[Ljava.lang.Object;" data (.data m)]
      (aset data x v)))
  (set-2d! [m x y v]
    (when-not (= 2 (.ndims m))
      (throw (IllegalArgumentException. "can't use set-2d! on non-matrix")))
    (let [^longs strides (.strides m)
          #^"[Ljava.lang.Object;" data (.data m)
          idx (+ (* (aget strides 0) (long x))
                 (* (aget strides 1) (long y)))]
      (aset data idx v)))
  (set-nd! [m indexes v]
    (when-not (= (count indexes) (.ndims m))
      (throw (IllegalArgumentException.
              "index count should match dimensionality")))
    (let [idxs (long-array indexes)
          strides (.strides m)
          #^"[Ljava.lang.Object;" data (.data m)
          idx (get-strided-idx idxs strides)]
      (aset data idx v))))

;; PMatrixCloning requires only "clone" method, which is used to clone
;; mutable matrix. The mutation of clone must not affect the original.
;; TODO: an open question is whether we need to normalize memory layout here
;; (forcing data to conform C-order, for example) or not

(extend-type NDArray
  mp/PMatrixCloning
  (clone [m]
    (let [#^"[Ljava.lang.Object;" data-old (.data m)
          data (aclone data-old)
          ndims (.ndims m)
          shape (aclone (longs (.shape m)))
          strides (aclone (longs (.strides m)))]
      (NDArray. data ndims shape strides))))

;; TODO: one can't register an implementation without implementing
;; PConversion first. Seems wrong to me.
;;
;; (extend-type NDArray
;;   mp/PConversion
;;   (convert-to-nested-vectors [m]
;;     (cond
;;      (== 0 (alength (.shape m)))
;;      (aget (.data m) 0)
;;      (== 1 (alength (.shape m)))
;;      (into [] (.data m))
;;      :else
;;      (mapv mp/convert-to-nested-vectors (mp/get-major-slice-seq m))))

;;   #_java.lang.Object
;;   #_(toString [m]
;;     (str (mp/persistent-vector-coerce m)))
;;   )

;; Register implementation

#_(imp/register-implementation (empty-ndarray [1]))

;; ## Links
;; [1] http://docs.scipy.org/doc/numpy/reference/arrays.ndarray.html
;; [2] http://scipy-lectures.github.io/advanced/advanced_numpy/
;; [3] http://clj-me.cgrand.net/2009/08/06/what-warn-on-reflection-doesnt-tell-you-about-arrays/