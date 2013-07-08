(ns clojure.core.matrix.docgen.implementations
  (:use [clojure.java.shell :only [sh]])
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as mi]
            [hiccup.page :as h]))

;; ## Info
;; This file provides rather hacky solution for generating
;; html documentation about various matrix implementations available in
;; core.matrix

(def repo-url "https://github.com/mikera/matrix-api")
(def src-path "src/main/clojure")

(defn protocol?
  "Returns true if an argument is a protocol'"
  [p]
  (and (map? p)
       (:on-interface p)
       (.isInterface (:on-interface p))))

(defn enhance-protocol-kv
  "Transform MapEntry to just map with some additional fields"
  [[name p]]
  (let [m (->> @p :var meta)]
    (assoc @p :line (:line m) :file (:file m) :name name)))

(defn extract-protocols
  "Extracts protocol info from clojure.core.matrix.protocols"
  []
  (->> (ns-publics 'clojure.core.matrix.protocols)
       (filter (comp protocol? deref val))
       (map enhance-protocol-kv)
       (sort-by :line)))

(defn get-impl-objs
  "Returns a list of available implementations' objects"
  []
  (filter second
          (for [[name ns] mi/KNOWN-IMPLEMENTATIONS
                ;; [name ns] {:vectorz 'mikera.vectorz.matrix-api
                ;;            :ndarray 'clojure.core.matrix.impl.ndarray
                ;;            :scalar-wrapper 'clojure.core.matrix.impl.wrappers
                ;;            :slice-wrapper 'clojure.core.matrix.impl.wrappers
                ;;            :nd-wrapper 'clojure.core.matrix.impl.wrappers
                ;;            :persistent-vector 'clojure.core.matrix.impl.persistent-vector
                ;;            }
                :when (not (#{:TODO :persistent-vector} ns))]
            (try
              {:name name, :obj (mi/get-canonical-object name)}
              (catch Throwable t nil)))))

(defn find-implementers
  "Returns a set of implementation names of implementations that
   support provided protocol"
  [protocol impl-objs]
  (->> impl-objs
       (filter #(->> % :obj class (extends? protocol)))
       (map :name)
       (into #{})))

(defn extract-implementations
  "Returns a a sequence of protocol maps augmented with :implemented-by key
   that contains a set of names of supporting implementations"
  [protocols impl-objs]
  (for [proto protocols]
    (assoc proto :implemented-by (find-implementers proto impl-objs))))

(defn get-git-hash
  "Returns current revision's git hash"
  []
  (-> (sh "git" "log" "--pretty=format:'%H'" "-n 1")
      :out
      (clojure.string/replace #"'" "")))


(defn render-header
  [git-hash]
  (seq [[:h2 "Protocol implementation summary"]
        [:p "git hash: "
         [:a {:href (str repo-url "/blob/" git-hash)}
          git-hash]]]))

(defn render-protocol
  [p git-hash]
  (let [src-href (str repo-url "/blob/" git-hash "/" src-path "/"
                      (:file p) "#L" (:line p))]
    (seq [(:name p) "&nbsp;"
          [:small "(line " [:a {:href src-href} (:line p)] ")" ]])))

(defn render-table
  [impl-names protos git-hash]
  [:table.pure-table
   [:thead
    [:th]
    (for [impl-name impl-names] [:th (name impl-name)])]
   [:tbody
    (for [[i p] (map-indexed vector protos)]
      [:tr {:class (if (even? i) "pure-table-odd")}
       [:td (render-protocol p git-hash)]
       (for [impl-name impl-names]
         [:td {:style "text-align: center;"}
          (when ((:implemented-by p) impl-name)
            [:i.icon-ok.icon-large])])])]])

(defn render-page
  [header table]
  (h/html5
     [:head
      [:title "Protocol/Implementation summary"]
      (h/include-css "http://yui.yahooapis.com/pure/0.2.0/pure-min.css")
      (h/include-css "http://netdna.bootstrapcdn.com/font-awesome/3.2.1/css/font-awesome.css")]
     [:body {:style "padding: 0 2em;"}
      [:div.pure-g
       [:div.pure-u-1 header]
       [:div.pure-u-1 table]]]))

(defn generate
  []
  (let [impl-objs (get-impl-objs)
        impl-names (map :name impl-objs)
        protos (extract-implementations (extract-protocols) impl-objs)
        git-hash (get-git-hash)
        header (render-header git-hash)
        table (render-table impl-names protos git-hash)
        page (render-page header table)]
    (prn impl-names)
    (h/html5 page)))