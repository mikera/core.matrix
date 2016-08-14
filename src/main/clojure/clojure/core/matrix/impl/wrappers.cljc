(ns clojure.core.matrix.impl.wrappers
  "Implementations for specialised wrapper types.

   These wrapper types enable efficient of convenient implementation of various core.matrix protocols."
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as imp]
            [clojure.core.matrix.impl.persistent-vector]
            [clojure.core.matrix.utils :as u])
  #?(:clj (:require
            [clojure.core.matrix.macros :refer [TODO error]]
            [clojure.core.matrix.macros-clj :refer [abutnth areverse]])
     :cljs (:require-macros
             [clojure.core.matrix.macros :refer [TODO error c-for]]
             [clojure.core.matrix.macros-cljs :refer [abutnth areverse]]
             [clojure.core.matrix.impl.wrappers :refer [set-source-index]]))
  #?(:clj
      (:import [clojure.lang Seqable Indexed])))

;; =============================================
;; WRAPPER IMPLEMENTATIONS
;;
;; wrappers are used to implement specific shapes / types of arrays
;; that are useful to implement certain array operations (typically as return values)

#?(:clj (do
(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
))

(declare wrap-slice wrap-nd wrap-scalar)

;; =============================================
;; ScalarWrapper
;;
;; wraps a single scalar as a mutable 0-D array

(deftype ScalarWrapper [^{:volatile-mutable true} value]
  Object
    (toString [m] (str value))

  mp/PImplementation
    (implementation-key [m]
      :scalar-wrapper)
    ;; we delegate to persistent-vector implementation for new matrices.
    (meta-info [m]
      {:doc "Wraps a single scalar as a mutable 0-D array"})
    (new-vector [m length]
      (mp/new-vector [] length))
    (new-matrix [m rows columns]
      (mp/new-matrix [] rows columns))
    (new-matrix-nd [m dims]
      (mp/new-matrix-nd [] dims))
    (construct-matrix [m data]
      (if (== 0 (long (mp/dimensionality data)))
        (if (mp/is-scalar? data)
          (ScalarWrapper. data)
          (ScalarWrapper. (mp/get-0d data)))
        (mp/clone data)))
    (supports-dimensionality? [m dims]
      (== (long dims) 0))

  mp/PDimensionInfo
    (dimensionality [m]
      0)
    (get-shape [m]
      [])
    (is-scalar? [m]
      false) ;; note that a ScalarWrapper is not itself a scalar!!
    (is-vector? [m]
      false)
    (dimension-count [m dimension-number]
      (error "Can't get dimension-count of ScalarWrapper: no dimensions exist"))

  mp/PIndexedAccess
    (get-1d [m row]
      (error "Can't get-1d on ScalarWrapper."))
    (get-2d [m row column]
      (error "Can't get-2d on ScalarWrapper."))
    (get-nd [m indexes]
      (if (seq indexes)
        (error "Can't get-1d on ScalarWrapper.")
        value))

  mp/PIndexedSetting
    (set-1d [m x v]
      (error "Can't do 1D set on 0D array"))
    (set-2d [m x y v]
      (error "Can't do 2D set on 0D array"))
    (set-nd [m indexes v]
      (if (not (seq indexes))
        (ScalarWrapper. v)
        (error "Can't set on 0D array with dimensionality: " (count indexes))))
    (is-mutable? [m] true)

  ;; in nested vector format, we don't want the wrapper....
  mp/PConversion
    (convert-to-nested-vectors [m]
      value)

  mp/PZeroDimensionAccess
    (get-0d [m]
      value)
    (set-0d! [m v]
      (set! value v))

  mp/PMatrixCloning
    (clone [m] (ScalarWrapper. value)))

;; =============================================
;; SliceWrapper
;;
;; wraps a row-major slice of an array

(deftype SliceWrapper [array ^long slice]
  #?(:clj Seqable :cljs ISeqable)
    (#?(:clj seq :cljs -seq) [m]
      (mp/get-major-slice-seq m))

  mp/PImplementation
    (implementation-key [m]
      :slice-wrapper)
    ;; we delegate to persistent-vector implementation for new matrices.
    (meta-info [m]
      {:doc "Wraps a row-major slice of an array"})
    (new-vector [m length]
      (mp/new-vector [] length))
    (new-matrix [m rows columns]
      (mp/new-matrix [] rows columns))
    (new-matrix-nd [m dims]
      (mp/new-matrix-nd [] dims))
    (construct-matrix [m data]
      (mp/construct-matrix [] data))
    (supports-dimensionality? [m dims]
      true)

  mp/PDimensionInfo
    (dimensionality [m]
      (dec (long (mp/dimensionality array))))
    (get-shape [m]
      (next (mp/get-shape array)))
    (is-scalar? [m]
      false)
    (is-vector? [m]
      (== 2 (long (mp/dimensionality array)))) ;; i.e. the slice has dimensionality 1
    (dimension-count [m dimension-number]
      (let [dimension-number (long dimension-number)]
        (if (< dimension-number 0)
         (error "Can't access negative dimension!")
         (mp/dimension-count array (inc dimension-number)))))

  mp/PIndexedAccess
    (get-1d [m row]
      (mp/get-2d array slice row))
    (get-2d [m row column]
      (mp/get-nd array [slice row column]))
    (get-nd [m indexes]
      (mp/get-nd array (cons slice indexes)))

  mp/PZeroDimensionAccess
    (get-0d [m]
      (mp/get-1d array slice))
    (set-0d! [m value]
      (mp/set-1d! array slice value))

  mp/PConversion
    (convert-to-nested-vectors [m]
      (if (mp/is-vector? array)
        (mp/get-1d array slice)
        (mapv mp/convert-to-nested-vectors (mp/get-major-slice-seq m))))

  mp/PIndexedSetting
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
      (mp/is-mutable? array)) ;; i.e. is the underlying array mutable

  mp/PIndexedSettingMutable
    (set-1d! [m row v]
      (mp/set-2d! array slice row v))
    (set-2d! [m row column v]
      (mp/set-nd! array [slice row column] v))
    (set-nd! [m indexes v]
      (mp/set-nd! array (cons slice indexes) v))

  mp/PMatrixCloning
    (clone [m] (wrap-slice (mp/clone array) slice))

  Object
    (toString [m] (str (mp/convert-to-nested-vectors m))))


;; =============================================
;; NDWrapper
;;
;; wraps an N-dimensional subset or broadcast of an array
;; supports aritrary permutations of dimensions and indexes
#?(:clj
(defmacro set-source-index
  "Sets up an index into the source vector for dimension i at position val"
  [ix i val]
  (let [isym (gensym "i")]
    `(let [~isym ~i
           tdim# (aget ~'dim-map ~isym)]
       (when (>= tdim# 0)
         (aset ~ix tdim# (aget ~(vary-meta `(aget ~'index-maps ~isym) assoc :tag 'longs) ~val))))))
)

(deftype NDWrapper
  [array ;; source array (any valid core.matrix matrix)
   ^longs shape ;; shape of NDWrapper
   ^longs dim-map ;; map of NDWrapper dimensions to source dimensions
   ^objects index-maps ;; maps of each NDWrapper dimension's indexes to source dimension indexes
   ^longs source-position ;; position in source array for non-specified dimensions
   ]
  #?(:clj Seqable :cljs ISeqable)
    (#?(:clj seq :cljs -seq) [m]
      (mp/get-major-slice-seq m))

  #?(:clj Indexed :cljs IIndexed)
    (#?(:clj nth :cljs -nth) [m i]
      (mp/get-major-slice m i))

    (#?(:clj nth :cljs -nth) [m i not-found]
      (if (and (integer? i) (<= 0 i) (< i (aget shape 0)))
        (mp/get-major-slice m i)
        not-found))

    #?(:cljs ICounted)
    (#?(:clj count :cljs -count) [m]
      (aget shape 0))

  mp/PImplementation
    (implementation-key [m]
      :nd-wrapper)
    ;; we delegate to persistent-vector implementation for new matrices.
    (meta-info [m]
      {:doc "Wraps an N-dimensional subset or broadcast of an array"})
    (new-vector [m length]
      (mp/new-vector [] length))
    (new-matrix [m rows columns]
      (mp/new-matrix [] rows columns))
    (new-matrix-nd [m dims]
      (mp/new-matrix-nd [] dims))
    (construct-matrix [m data]
      (mp/construct-matrix [] data))
    (supports-dimensionality? [m dims]
      true)

  mp/PIndexedSetting
    (set-1d [m x v]
      (mp/set-1d (mp/coerce-param (imp/get-canonical-object) m) x v))
    (set-2d [m x y v]
      (mp/set-2d (mp/coerce-param (imp/get-canonical-object) m) x y v))
    (set-nd [m indexes v]
      (mp/set-nd (mp/coerce-param (imp/get-canonical-object) m) indexes v))
    (is-mutable? [m] (mp/is-mutable? array))

  mp/PSubVector
    (subvector [m start length]
      (when (not= 1 (alength shape)) (error "Can't take subvector: wrong dimensionality = " (alength shape)))
      (let [vlen (long (aget shape 0))
            start (long start)
            length (long length)
            end (+ start length)
            ^longs old-index-map (aget index-maps 0)
            ^longs new-index-map (long-array length)]
        (when (< start 0) (error "Start index out of bounds: " start))
        (when (> end vlen) (error "End index out of bounds: " end))
        (dotimes [i length]
          (aset new-index-map i (aget old-index-map (+ start i))))
        (NDWrapper.
          array
          (u/long-array-of length)
          dim-map
          (u/object-array-of new-index-map)
          (u/copy-long-array source-position))))

  mp/PDimensionInfo
    (dimensionality [m]
      (alength shape))
    (get-shape [m]
      shape)
    (is-scalar? [m]
      false)
    (is-vector? [m]
      (== 1 (alength shape)))
    (dimension-count [m dimension-number]
      (aget shape (int dimension-number)))

  mp/PZeroDimensionAccess
    (get-0d [m]
      (mp/get-nd array source-position))
    (set-0d! [m value]
      (mp/set-nd! array source-position value))

  mp/PIndexedAccess
    (get-1d [m row]
      (let [ix (u/copy-long-array source-position)
            ^longs im (aget index-maps 0)]
        (set-source-index ix 0 row)
        (mp/get-nd array ix)))
    (get-2d [m row column]
      (let [ix (u/copy-long-array source-position)]
        (set-source-index ix 0 row)
        (set-source-index ix 1 column)
        (mp/get-nd array ix)))
    (get-nd [m indexes]
      (let [^longs ix (u/copy-long-array source-position)]
        (dotimes [i (alength shape)]
          (set-source-index ix i (nth indexes i)))
        (mp/get-nd array ix)))

    mp/PIndexedSettingMutable
    (set-1d! [m row v]
      (let [ix (u/copy-long-array source-position)
            ^longs im (aget index-maps 0)]
        (set-source-index ix 0 row)
        (mp/set-nd! array ix v)))
    (set-2d! [m row column v]
      (let [ix (u/copy-long-array source-position)]
        (set-source-index ix 0 row)
        (set-source-index ix 1 column)
        (mp/set-nd! array ix v)))
    (set-nd! [m indexes v]
      (let [^longs ix (u/copy-long-array source-position)
            n (alength shape)]
        (when (not= n (count indexes))
          (error "set-nd! called with index " (vec indexes) " indexes on wrapped array of shape " shape))
        (dotimes [i (alength shape)]
          (set-source-index ix i (nth indexes i)))
        (mp/set-nd! array ix v)))

    mp/PSliceView2
      (get-slice-view [m dim i]
        (let [i (long i)
              dim (long dim)
              nsp (u/copy-long-array source-position)
              sdim (long (aget dim-map dim))]
          (aset nsp sdim i)
          (NDWrapper. array
                      (abutnth dim shape)
                      (abutnth dim dim-map)
                      (abutnth dim index-maps)
                      nsp)))

  Object
    (toString [m]
      (str (mp/persistent-vector-coerce m))))

(defn wrap-slice
  "Creates a view of a major slice of an array."
  ([m slice]
    (let [slice (long slice)]
      (when (>= slice (long (mp/dimension-count m 0)))
        (error "Slice " slice " does not exist on " (#?(:clj class :cljs type) m)))
      (SliceWrapper. m slice))))

(defn wrap-nd
  "Wraps an array in a NDWrapper view. Useful for taking submatrices, subviews etc."
  ([m]
      (let [shp (long-array (mp/get-shape m))
            dims (alength shp)]
        (NDWrapper. m
                  shp
                  (u/long-range dims)
                  (object-array (map #(u/long-range (mp/dimension-count m %)) (range dims)))
                  (long-array dims)))))

(defn wrap-selection
  "Wraps an array using a selection of indexes for each dimension."
  [m indices]
  (let [shp (long-array (map count indices))
        dims (count shp)]
    (NDWrapper.
      m
      shp
      (long-array (range dims))
      (object-array (map long-array indices))
      (long-array (repeat dims 0)))))

(defn wrap-submatrix
  "Wraps an array using a selection of [start length] ranges for each dimension."
  [m dim-ranges]
  (let [shp (mp/get-shape m)
        dims (count shp)
        _ (if-not (== dims (count dim-ranges)) (error "submatrix ranges " dim-ranges " do not match matrix dimensionality of " shp))
        dim-ranges (mapv (fn [a cnt] (if a (vec a) [0 cnt])) dim-ranges shp)
        new-shape (long-array (map (fn [[start len]] len) dim-ranges))]
    (NDWrapper.
      m
      new-shape
      (long-array (range (count shp)))
      (object-array
        (map (fn [[^long start ^long len]] (long-array (range start (+ start len))))
             dim-ranges))
      (long-array (repeat dims 0)))))

(defn wrap-broadcast
  "Wraps an array with broadcasting to the given target shape."
  [m target-shape]
  (let [tshape (long-array target-shape)
        tdims (alength tshape)
        mshape (long-array (mp/get-shape m))
        mdims (alength mshape)
        dim-map (long-array (concat (repeat (- tdims mdims) -1) (range mdims)))]
    (NDWrapper.
      m
      tshape
      dim-map
      (object-array
        (for [i (range tdims)]
          (let [i (long i)
                arr (long-array (aget tshape i))
                mdim (- i (- tdims mdims))]
            (when (>= mdim 0)
              (let [mdc (aget mshape mdim)
                    tdc (aget tshape i)]
                (cond
                  (== mdc 1) nil
                  (== mdc tdc) (dotimes [i mdc] (aset arr i i))
                  :else (error "Can't broadcast shape " (seq mshape)
                               " to target shape " (seq tshape)))))
            arr)))
      (long-array mdims))))

(defn wrap-scalar
  "Wraps a scalar value into a mutable 0D array."
  ([m]
      (cond
        (mp/is-scalar? m)
          (ScalarWrapper. m)
        :else
          (ScalarWrapper. (mp/get-0d m)))))

;; note we construct these types directly because default implementations are not yet loaded
;; i.e. the wrap-xxxx functions will probably not yet work
(imp/register-implementation (ScalarWrapper. 13))

(imp/register-implementation (NDWrapper. [1]
                                         (long-array 0)
                                         (long-array 0)
                                         (object-array 0)
                                         (long-array [0])))

;(imp/register-implementation (SliceWrapper. [1 2] 0))
