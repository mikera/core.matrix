(ns clojure.core.matrix.docgen.bench
  (:use [clojure.core.matrix.docgen.bench-suite])
  (:require [criterium.core :as cr]
            [clojure.string :as s]
            [hiccup.page :as h]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.utils :as utils]
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

;; TODO: remove enumerated usage, use map-indexed instead
(defn enumerated [xs]
  (map vector (range (count xs)) xs))

(defn format-elapsed [t]
  (if (< t Double/POSITIVE_INFINITY)
    (let [[factor unit] (cr/scale-time t)]
      (format "%2.2f %s" (* factor t) unit))
    "t/o"))

(defn hsl-to-rgb [[h s l]]
  (let [h' (/ h 60.0)
        c (* s (- 1 (Math/abs (- (* 2 l) 1))))
        x (* c (- 1.0 (Math/abs (- (mod h' 2) 1.0))))
        m (- l (* 0.5 c))
        unnormed (map #(+ m %)
                      (cond (nil? h) [0, 0, 0]
                            (and (>= h' 0) (< h' 1)) [c x 0]
                            (and (>= h' 1) (< h' 2)) [x c 0]
                            (and (>= h' 2) (< h' 3)) [0 c x]
                            (and (>= h' 3) (< h' 4)) [0 x c]
                            (and (>= h' 4) (< h' 5)) [x 0 c]
                            (and (>= h' 5) (< h' 6)) [c 0 x]))]
    (map #(* % 255.0) unnormed)))

(defn rgb-to-css-color [ls]
  (str "#" (reduce #(str % (format "%02X" (int %2))) "" ls)))

(defn colorize [is-odd x]
  (let [[min-s max-s] [0.0 1.0]
        [min-l max-l] (if is-odd [0.88 0.95] [0.9 1.0])
        ds (- max-s min-s)
        dl (- max-l min-l)
        x-gamma (Math/pow x 1.5)
        hsl-color [0 (- max-s (* x-gamma ds)) (+ min-l (* x-gamma dl))]]
    (-> hsl-color hsl-to-rgb rgb-to-css-color)))

;; # State

(defonce bench-results (atom {}))

;; # Rendering

(defn render-header
  [git-hash]
  (seq [[:h2 "Benchmark summary"]
        [:p "git hash: "
         [:a {:href (str repo-url "/blob/" git-hash)}
          git-hash]]
        [:small "Hint: hover on method name to see an additional information"]]))

;; TODO: output quantiles/variance
(defn render-bench-results [min-times bench-res is-odd]
  [:div
   (for [size [:s :m :l]]
     (if-let [elapsed (size bench-res)]
       [:div
        {:style (str "background-color: "
                     (colorize is-odd (/ (size min-times) elapsed)))}
        (format-elapsed elapsed)]
       [:div "&nbsp;"]))])

(defn find-min-times [f-bench]
  (when (seq f-bench)
    (let [min (fnil min Double/POSITIVE_INFINITY Double/POSITIVE_INFINITY)]
      (reduce (fn [{min-s :s min-m :m min-l :l :as acc}
                   [_ {:keys [s m l]}]]
                (assoc acc
                  :s (min min-s s)
                  :m (min min-m m)
                  :l (min min-l l)))
              {:s Double/POSITIVE_INFINITY
               :m Double/POSITIVE_INFINITY
               :l Double/POSITIVE_INFINITY}
              f-bench))))

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
             :let [f-name-kw (keyword f-name)
                   min-times (find-min-times (-> @bench-results f-name-kw))
                   is-odd (= (rem proto-i 2) 0)]]
         [:tr
          (when is-odd {:class "pure-table-odd"})
          (when (= f-i 0)
            [:td {:rowspan (count (:sigs proto))}
             [:strong (:name proto)]])
          [:td
           (let [additional-info
                 (when-let [varying (-> @benches ((keyword f-name)) :varying)]
                   (str "Varying: " varying))]
             [:span
              (when additional-info
                {:title additional-info})
              f-name])]
          (for [[a-type a-info] array-types]
            [:td.bench-results
             (when-let [bench-result (-> @bench-results f-name-kw a-type)]
               (render-bench-results min-times bench-result is-odd))])]))]]])

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
    #_(for [i (range 11)]
      [:div {:style (str "background-color: "
                         (dec-to-hex-str (hsl-to-rgb (colorize (/ i 10.0)))))}
       i])
    [:div.pure-g
     [:div.pure-u-1 header]
     [:div.pure-u-1 table]
     [:script {:type "text/javascript"}
      "$('#benchtable').stickyTableHeaders();"]]]))

(declare ^:dynamic dyn-args)

(defn make-bench [f-name array-type to-convert bench]
  (letfn [(args-prepare [constructor args]
            (map (fn [[i arg]] (if (to-convert i) (constructor arg) arg))
                 (enumerated args)))
          (construct-bench-call [f args-name args]
            (let [arg-names (map (fn [_] (gensym)) args)
                  arg-vals (map (fn [i] `(nth ~args-name ~i))
                                (range (count args)))
                  arg-pairs (interleave arg-names arg-vals)]
              `(let [~@arg-pairs]
                 (cr/quick-benchmark (~f ~@arg-names) {}))))]
    (binding [cr/*final-gc-problem-threshold* 0.5]
      (let [f (ns-resolve 'clojure.core.matrix.protocols f-name)
            constructor (-> array-types array-type :constructor)
            arg-sets (for [[size args] bench]
                       [size ])]
        (into {} (for [[size args-raw] bench
                       :let [args (args-prepare constructor args-raw)
                             bench-call (construct-bench-call f 'dyn-args args)]]
                   [size (->> (binding [dyn-args args]
                                (eval bench-call))
                              :mean
                              first)]))))))

(defn perform-bench []
  (doseq [proto (utils/extract-protocols)]
    (doseq [[_ {f-name :name}] (:sigs proto)
            :let [f-name-kw (keyword f-name)]]
      (doseq [[a-type a-info] array-types]
        (when-let [bench (-> @benches f-name-kw a-type)]
          (when-not (-> @bench-results f-name-kw a-type)
            (println "benchmarking" f-name "on" (name a-type))
            (swap! bench-results assoc-in [f-name-kw a-type]
                   (make-bench f-name
                               a-type
                               (-> @benches f-name-kw :to-convert)
                               (force bench))))))))
  :ok)

(defn dump-bench-results
  ([] (dump-bench-results "bench.dump"))
  ([fname]
     (binding [*print-dup* true]
       (->> @bench-results
            pr-str
            (spit fname)))))

(defn load-bench-results
  ([] (load-bench-results "bench.dump"))
  ([fname]
     (->> fname
          slurp
          read-string
          (reset! bench-results))
     :ok))

(defn generate []
  (let [git-hash (c/get-git-hash)
        protos (utils/extract-protocols)
        header (render-header git-hash)
        table (render-table protos)]
    (render-page header table)))
