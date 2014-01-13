(ns clojure.core.matrix.impl.wrappers
  (:require [clojure.core.matrix.protocols :as mp])
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.multimethods :as mm]))

;; =============================================
;; WRAPPER IMPLEMENTATIONS
;;
;; wrappers are used to implement specific shapes / types of arrays
;; that are useful to implement certain array operations (typically as return values)

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(declare wrap-slice wrap-nd wrap-scalar)

;; =============================================
;; ScalarWrapper
;;
;; wraps a single scalar as a mutable 0-D array

(deftype ScalarWrapper [^{:volatile-mutable true} value]
  java.lang.Object
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
      (if (== 0 (mp/dimensionality data))
        (if (mp/is-scalar? data)
          (ScalarWrapper. data)
          (ScalarWrapper. (mp/get-0d data)))
        (mp/clone data)))
    (supports-dimensionality? [m dims]
      (== dims 0))

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
  clojure.lang.Seqable
    (seq [m]
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
      (dec (mp/dimensionality array)))
    (get-shape [m]
      (next (mp/get-shape array)))
    (is-scalar? [m]
      false)
    (is-vector? [m]
      (== 2 (mp/dimensionality array))) ;; i.e. the slice has dimensionality 1
    (dimension-count [m dimension-number]
      (if (< dimension-number 0)
        (error "Can't access negative dimension!")
        (mp/dimension-count array (inc dimension-number))))

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

  java.lang.Object
    (toString [m] (str (mp/convert-to-nested-vectors m))))


;; =============================================
;; NDWrapper
;;
;; wraps an N-dimensional subset or broadcast of an array
;; supports aritrary permutations of dimensions and indexes

(defmacro set-source-index [ix i val]
  "Sets up an index into the source vector for dimension i at position val"
  (let [isym (gensym "i")]
    `(let [~isym ~i
           tdim# (aget ~'dim-map ~isym)]
       (when (>= tdim# 0)
         (aset ~ix tdim# (aget ~(vary-meta `(aget ~'index-maps ~isym) assoc :tag 'longs) ~val))))))

(deftype NDWrapper
  [array ;; source array (any valid core.matrix matrix)
   ^longs shape ;; shape of NDWrapper
   ^longs dim-map ;; map of NDWrapper dimensions to source dimensions
   ^objects index-maps ;; maps of each NDWrapper dimension's indexes to source dimension indexes
   ^longs source-position ;; position in source array for non-specified dimensions
   ]
  clojure.lang.Seqable
    (seq [m]
      (mp/get-major-slice-seq m))

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
      (TODO))
    (set-2d [m x y v]
      (TODO))
    (set-nd [m indexes v]
      (TODO))
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
          (long-array-of length)
          dim-map
          (object-array-of new-index-map)
          source-position)))

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
      (mp/set-nd array source-position value))

  mp/PIndexedAccess
    (get-1d [m row]
      (let [ix (copy-long-array source-position)
            ^longs im (aget index-maps 0)]
        (set-source-index ix 0 row)
        (mp/get-nd array ix)))
    (get-2d [m row column]
      (let [ix (copy-long-array source-position)]
        (set-source-index ix 0 row)
        (set-source-index ix 1 column)
        (mp/get-nd array ix)))
    (get-nd [m indexes]
      (let [^longs ix (copy-long-array source-position)]
        (dotimes [i (alength shape)]
          (set-source-index ix i (nth indexes i)))
        (mp/get-nd array ix)))


  java.lang.Object
    (toString [m]
      (str (mp/persistent-vector-coerce m))))

(defn wrap-slice
  "Creates a view of a major slice of an array."
  ([m slice]
    (let [slice (long slice)]
      (when (>= slice (mp/dimension-count m 0)) (error "Slice " slice " does not exist on " (class m)))
      (SliceWrapper. m slice))))

(defn wrap-nd
  "Wraps an array in a NDWrapper view. Useful for taking submatrices, subviews etc."
  ([m]
      (let [shp (long-array (mp/get-shape m))
            dims (alength shp)]
        (NDWrapper. m
                  shp
                  (long-range dims)
                  (object-array (map #(long-range (mp/dimension-count m %)) (range dims)))
                  (long-array (repeat dims 0))))))

(defn wrap-submatrix
  [m dim-ranges]
  (let [shp (mp/get-shape m)
        dims (count shp)
        _ (if-not (== dims (count dim-ranges)) (error "submatrix ranges do not match matrix dimensionality"))
        dim-ranges (mapv (fn [a cnt] (if a (vec a) [0 cnt])) dim-ranges shp)
        new-shape (long-array (map (fn [[start len]] len) dim-ranges))]
    (NDWrapper.
      m
      new-shape
      (long-array (range (count shp)))
      (object-array
        (map (fn [[start len]] (long-array (range start (+ start len))))
             dim-ranges))
      (long-array (repeat dims 0)))))

(defn wrap-broadcast
  "Wraps an array with broadcasting to the given target shape."
  [m target-shape]
  (let [tshape (long-array target-shape)
        tdims (count tshape)
        mshape (long-array (mp/get-shape m))
        mdims (count mshape)
        dim-map (long-array (concat (repeat (- tdims mdims) -1) (range mdims)))]
    ;;(println "mshape:" (seq mshape))
    ;;(println "mdims:" mdims)
    ;;(println "tdims:" tdims)
    (NDWrapper.
      m
      tshape
      dim-map
      (object-array
        (for [i (range tdims)]
          (let [arr (long-array (aget tshape i))
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

(imp/register-implementation (SliceWrapper. [1 2] 0))