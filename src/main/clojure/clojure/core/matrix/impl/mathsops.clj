(ns clojure.core.matrix.impl.mathsops)

;; data structure for code generation of maths operations
;; format is (<symbol> <java.lang.Math equivalent>)

(def maths-ops
  '[(abs Math/abs)
	  (acos Math/acos)
	  (asin Math/asin)
	  (atan Math/atan)
	  (cbrt Math/cbrt)
	  (ceil Math/ceil)
	  (cos Math/cos)
	  (cosh Math/cosh)
	  (exp Math/exp)
	  (floor Math/floor)
	  (log Math/log)
	  (log10 Math/log10)
	  (round Math/rint)
	  (signum Math/signum)
	  (sin Math/sin)
	  (sinh Math/sinh)
	  (sqrt Math/sqrt)
	  (tan Math/tan)
	  (tanh Math/tanh)
   	(to-degrees Math/toDegrees)
	  (to-radians Math/toRadians)])

;; define double versions of maths-ops
(doseq [[sym op] maths-ops]
  (let []
    (eval `(defn ~sym 
            ~(vary-meta
               `([~(vary-meta 'x assoc :tag 'double)]
                  (~op ~'x))
               assoc :tag double)))))