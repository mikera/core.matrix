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

(def repo-url "https://github.com/mikera/matrix-api")
(def src-path "src/main/clojure")

(def conf {:tobench [:vectorz :ndarray]
           :reference :vectorz
           :fast-bench true})

(defn rand-mtx
  ([n] (rand-mtx n 1000 identity))
  ([n max caster]
     (letfn [(rand-row [] (->> #(rand-int max)
                               repeatedly (map caster) (take n) vec))]
       (->> rand-row repeatedly (take n) vec))))

(def tests
  (array-map
   :vectorz {:name "vectorz"
             :constructor #(m/array :vectorz %)
             :counts [5 50 500 1000]}
   :vecs {:name "persistent vectors"
          :constructor identity
          :counts [5 50]}
   :ndarray {:name "ndarray"
             :constructor #(m/array :ndarray %)
             :counts [5 50]}
   :ndarray-double {:name "ndarray-double"
                    :constructor #(m/array :ndarray-double %)
                    :counts [5 50 500 1000]}))

(def array-types
  (array-map
   :vectorz {:name "vectorz"
             :constructor #(m/array :vectorz %)}
   :ndarray {:name "ndarray"
             :constructor #(m/array :ndarray %)}
   :ndarray-double {:name "ndarray-double"
                    :constructor #(m/array :ndarray-double %)}
   :vecs {:name "persistent vectors"
          :constructor identity}))

(def tests
  {:matrix-multiply
   {:varying "matrix sizes"
    :vectorz
    {:s [(rand-mtx 5) (rand-mtx 5)]
     :m [(rand-mtx 50) (rand-mtx 50)]
     :l [(rand-mtx 500) (rand-mtx 500)]}
    :vecs
    {:s [(rand-mtx 5) (rand-mtx 5)]
     :m [(rand-mtx 50) (rand-mtx 50)]}
    :ndarray
    {:s [(rand-mtx 5) (rand-mtx 5)]
     :m [(rand-mtx 50) (rand-mtx 50)]}
    :ndarray-double
    {:s [(rand-mtx 5) (rand-mtx 5)]
     :m [(rand-mtx 50) (rand-mtx 50)]
     :l [(rand-mtx 500) (rand-mtx 500)]}}

   :clone
   {:varying "2d matrix sizes"
    :vectorz
    {:s [(rand-mtx 5)]
     :m [(rand-mtx 50)]
     :l [(rand-mtx 500)]}
    :vecs
    {:s [(rand-mtx 5)]
     :m [(rand-mtx 50)]
     :l [(rand-mtx 500)]}
    :ndarray
    {:s [(rand-mtx 5)]
     :m [(rand-mtx 50)]
     :l [(rand-mtx 500)]}
    :ndarray-double
    {:s [(rand-mtx 5)]
     :m [(rand-mtx 50)]
     :l [(rand-mtx 500)]}}})

(defn mmultiply-bench []
  (doseq [[_ {:keys [name constructor counts]}] tests]
    (println (str name ":"))
    (doseq [n counts]
      (let [[ma mb] (->> #(rand-mtx n)
                         repeatedly (map constructor) (take 2))]
        (binding [cr/*final-gc-problem-threshold* 0.5]
          (->> (cr/quick-benchmark (mp/matrix-multiply ma mb) {})
               :mean
               (cr/report-point-estimate (str "n=" n ":"))))))))

(defn render-header
  [git-hash]
  (seq [[:h2 "Benchmark summary"]
        [:p "git hash: "
         [:a {:href (str repo-url "/blob/" git-hash)}
          git-hash]]
        [:small "Hint: hover on method name to see an additional information"]]))

(defn enumerated [xs]
  (map vector (range (count xs)) xs))

(defn make-bench [f-name array-type bench]
  (binding [cr/*final-gc-problem-threshold* 0.5]
    (let [f (resolve (symbol (str "mp/" f-name)))
          constructor (-> array-types array-type :constructor)
          arg-sets (for [[size args] bench]
                     [size (map constructor args)])]
      (into {} (for [[size args] arg-sets]
                 [size 0.0001 #_(->> (cr/quick-benchmark (apply f args) {})
                            :mean
                            first)])))))

(defn format-elapsed [t]
  (let [[factor unit] (cr/scale-time t)]
    (format "%2.2f %s" (* factor t) unit)))

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
       (for [[f-i [_ {f-name :name}]] (enumerated (:sigs proto))]
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
             (when-let [bench (-> tests ((keyword f-name)) a-type)]
               (render-bench-results
                (make-bench f-name a-type bench)))])]))]]])

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
      "$('#benchtable').stickyTableHeaders();"
      ]
     ]]))

(defn generate []
  (let [git-hash (c/get-git-hash)
        protos (c/extract-protocols)
        header (render-header git-hash)
        table (render-table protos)]
    (render-page header table)))