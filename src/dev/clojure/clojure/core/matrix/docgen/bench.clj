(ns clojure.core.matrix.docgen.bench
  (:require [criterium.core :as cr]
            [clojure.string :as s]
            [hiccup.page :as h]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as imp]
            [clojure.core.matrix.multimethods :as mm]
            [clojure.core.matrix.implementations :as mi]
            [clojure.core.matrix.docgen.common :as c]))

;; # Constants

(def repo-url "https://github.com/mikera/matrix-api")
(def src-path "src/main/clojure")

(def array-types
  (array-map
   :vectorz
   {:name "vectorz"
    :constructor #(m/array :vectorz %)}
   :ndarray
   {:name "ndarray"
    :constructor #(m/array :ndarray %)}
   :ndarray-double
   {:name "ndarray-double"
    :constructor #(m/array :ndarray-double %)}
   :vecs
   {:name "persistent vectors"
    :constructor identity}))

;; # Utils

(defn rand-vec
  ([n] (rand-vec n 1000 identity))
  ([n max] (rand-vec n max identity))
  ([n max caster]
     (->> #(rand-int max)
          repeatedly (map caster) (take n) vec)))

(defn rand-mtx
  ([n] (rand-mtx n 1000 identity))
  ([n max] (rand-mtx n max identity))
  ([n max caster]
     (letfn [(rand-row [] (->> #(rand-int max)
                               repeatedly (map caster) (take n) vec))]
       (->> rand-row repeatedly (take n) vec))))

(defn enumerated [xs]
  (map vector (range (count xs)) xs))

(defn format-elapsed [t]
  (if (< t Double/POSITIVE_INFINITY)
    (let [[factor unit] (cr/scale-time t)]
      (format "%2.2f %s" (* factor t) unit))
    "t/o"))

;; # State

(def benches (atom {}))
(defonce bench-results (atom {}))

;; # Benchmarks

(defn defbench [method varying to-convert
                & bench-pairs]
  (let [bench-sets (->> bench-pairs
                        (partition 2)
                        (mapcat (fn [[k v :as pair]]
                                  (if (keyword? k)
                                    [pair]
                                    (map vector k (repeat v)))))
                        (map vec))]
    (swap! benches assoc method
           (assoc (into {} bench-sets)
             :varying varying
             :to-convert to-convert))))

(defbench :matrix-multiply
  "matrix sizes"
  #{0 1}
  [:vectorz :ndarray-double]
  {:s [(rand-mtx 5)   (rand-mtx 5)]
   :m [(rand-mtx 50)  (rand-mtx 50)]
   :l [(rand-mtx 500) (rand-mtx 500)]}
  [:vecs :ndarray]
  {:s [(rand-mtx 5)  (rand-mtx 5)]
   :m [(rand-mtx 50) (rand-mtx 50)]})

(defbench :clone
  "2d matrix sizes"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)]
   :m [(rand-mtx 50)]
   :l [(rand-mtx 500)]})

(defbench :get-1d
  "1d vector size"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-vec 5)     (rand-int 5)]
   :m [(rand-vec 500)   (rand-int 500)]
   :l [(rand-vec 50000) (rand-int 5000)]})

(defbench :get-2d
  "2d matrix size"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   (rand-int 5)   (rand-int 5)]
   :m [(rand-mtx 50)  (rand-int 50)  (rand-int 50)]
   :l [(rand-mtx 500) (rand-int 500) (rand-int 500)]})

(defbench :get-nd
  "2d matrix size"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   [(rand-int 5)   (rand-int 5)]]
   :m [(rand-mtx 50)  [(rand-int 50)  (rand-int 50)]]
   :l [(rand-mtx 500) [(rand-int 500) (rand-int 500)]]})

(defbench :set-1d
  "1d vector size"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-vec 5)     (rand-int 5)    (rand-int 1000)]
   :m [(rand-vec 500)   (rand-int 500)  (rand-int 1000)]
   :l [(rand-vec 50000) (rand-int 5000) (rand-int 1000)]})

(defbench :set-2d
  "2d matrix size"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   (rand-int 5)   (rand-int 5)   (rand-int 1000)]
   :m [(rand-mtx 50)  (rand-int 50)  (rand-int 50)  (rand-int 1000)]
   :l [(rand-mtx 500) (rand-int 500) (rand-int 500) (rand-int 1000)]})

(defbench :set-nd
  "2d matrix size"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   [(rand-int 5)   (rand-int 5)]   (rand-int 1000)]
   :m [(rand-mtx 50)  [(rand-int 50)  (rand-int 50)]  (rand-int 1000)]
   :l [(rand-mtx 500) [(rand-int 500) (rand-int 500)] (rand-int 1000)]})

(defbench :set-1d!
  "1d vector size"
  #{0}
  [:vectorz :ndarray :ndarray-double]
  {:s [(rand-vec 5)     (rand-int 5)    (rand-int 1000)]
   :m [(rand-vec 500)   (rand-int 500)  (rand-int 1000)]
   :l [(rand-vec 50000) (rand-int 5000) (rand-int 1000)]})

(defbench :set-2d!
  "2d matrix size"
  #{0}
  [:vectorz :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   (rand-int 5)   (rand-int 5)   (rand-int 1000)]
   :m [(rand-mtx 50)  (rand-int 50)  (rand-int 50)  (rand-int 1000)]
   :l [(rand-mtx 500) (rand-int 500) (rand-int 500) (rand-int 1000)]})

(defbench :set-nd!
  "2d matrix size"
  #{0}
  [:vectorz :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   [(rand-int 5)   (rand-int 5)]   (rand-int 1000)]
   :m [(rand-mtx 50)  [(rand-int 50)  (rand-int 50)]  (rand-int 1000)]
   :l [(rand-mtx 500) [(rand-int 500) (rand-int 500)] (rand-int 1000)]})

(defn render-header
  [git-hash]
  (seq [[:h2 "Benchmark summary"]
        [:p "git hash: "
         [:a {:href (str repo-url "/blob/" git-hash)}
          git-hash]]
        [:small "Hint: hover on method name to see an additional information"]]))

;; TODO: output quantiles/variance
(defn render-bench-results [bench-res]
  [:div
   (for [size [:s :m :l]]
     [:div (if-let [elapsed (size bench-res)]
             (format-elapsed elapsed)
             "&nbsp;")])])

(defn render-table [protos]
  [:small
   [:table#benchtable.pure-table.pure-table-horizontal
    [:thead
     [:th "Protocol"]
     [:th "Method"]
     (for [[_ a-info] array-types]
       [:th.bench-results (:name a-info)])]
    [:tbody
     (for [[proto-i proto] (enumerated protos)]
       (for [[f-i [_ {f-name :name}]] (enumerated (:sigs proto))
             :let [f-name-kw (keyword f-name)]]
         [:tr
          (when (= (rem proto-i 2) 0)
            {:class "pure-table-odd"})
          (when (= f-i 0)
            [:td {:rowspan (count (:sigs proto))}
             [:strong (:name proto)]])
          [:td
           (let [additional-info
                 (when-let [varying (-> tests ((keyword f-name)) :varying)]
                   (str "Varying: " varying))]
             [:span
              (when additional-info
                {:title additional-info})
              f-name])]
          (for [[a-type a-info] array-types]
            [:td.bench-results
             (when-let [bench-result (-> @bench-results f-name-kw a-type)]
               (render-bench-results bench-result))])]))]]])

(defn render-page
  [header table]
  (h/html5
   [:head
    [:title "Benchmark summary"]
    [:meta {:charset "UTF-8"}]
    (h/include-css "http://yui.yahooapis.com/pure/0.2.0/pure-min.css")
    (h/include-css "http://netdna.bootstrapcdn.com/font-awesome/3.2.1/css/font-awesome.css")
    (h/include-js "http://code.jquery.com/jquery-2.0.3.min.js")
    (h/include-js "http://mikera.github.io/matrix-api/js/jquery.stickytableheaders.min.js")
    [:style {:type "text/css"}
     "td.bench-results {width: 8em; text-align:right;}"
     "th.bench-results {text-align: left; border-right: 1px solid #cbcbcb;  border-left: 1px solid #cbcbcb;}"
     ]
    ]
   [:body {:style "padding: 0 2em; font-family: sans-serif;"}
    [:div.pure-g
     [:div.pure-u-1 header]
     [:div.pure-u-1 table]
     [:script {:type "text/javascript"}
      "$('#benchtable').stickyTableHeaders();"]]]))

(defn make-bench [f-name array-type to-convert bench]
  (binding [cr/*final-gc-problem-threshold* 0.5]
    (let [f (resolve (symbol (str "mp/" f-name)))
          constructor (-> array-types array-type :constructor)
          arg-sets (for [[size args] bench]
                     [size (map (fn [[i arg]]
                                  (if (to-convert i)
                                    (constructor arg)
                                    arg))
                                (enumerated args))])]
      (into {} (for [[size args] arg-sets]
                 [size (->> (cr/quick-benchmark (apply f args) {})
                            :mean
                            first)])))))

(defn perform-bench []
  (doseq [proto (c/extract-protocols)]
    (doseq [[_ {f-name :name}] (:sigs proto)
          :let [f-name-kw (keyword f-name)]]
      (doseq [[a-type a-info] array-types]
        (when-let [bench (-> @benches f-name-kw a-type)]
          (swap! bench-results assoc-in [f-name-kw a-type]
                 (make-bench f-name
                             a-type
                             (-> @benches f-name-kw :to-convert)
                             bench))))))
  :ok)

(defn dump-bench-results [fname]
  (binding [*print-dup* true]
    (->> @bench-results
         pr-str
         (spit fname))))

(defn load-bench-results [fname]
  (->> fname
       slurp
       read-string
       (reset! bench-results))
  :ok)

(defn generate []
  (let [git-hash (c/get-git-hash)
        protos (c/extract-protocols)
        header (render-header git-hash)
        table (render-table protos)]
    (render-page header table)))