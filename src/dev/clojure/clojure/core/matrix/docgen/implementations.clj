(ns clojure.core.matrix.docgen.implementations
  (:use [clojure.java.shell :only [sh]])
  (:require [clojure.reflect :as r]
            [clojure.core.matrix.protocols :as mp]
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
                :when (not (#{:TODO :persistent-vector} ns))]
            (try
              {:name name, :obj (mi/get-canonical-object name)}
              (catch Throwable t nil)))))

(defn extends-deep?
  "This functions differs from ordinary `extends?` by using `extends?`
   on all ancestors of given type instead of just type itself. It also
   skips `java.lang.Object` that serves as a default implementation
   for all protocols"
  [proto cls]
  ;; Here we need a special case to avoid reflecting on primitive type
  ;; (it will cause an exception)
  (if (= (Class/forName "[D") cls)
    (extends? proto cls)
    (let [bases (-> cls (r/type-reflect :ancestors true) :ancestors)]
      (->> bases
           (filter (complement #{'java.lang.Object}))
           (map resolve)
           (cons cls)
           (map (partial extends? proto))
           (some true?)))))

(defn find-implementers
  "Returns a set of implementation names of implementations that
   support provided protocol"
  [protocol impl-objs]
  (->> impl-objs
       (filter #(->> % :obj class (extends-deep? protocol)))
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
          git-hash]]
        [:small "Hint: hover on protocol or implementation names to "
         "get their description"]]))

(defn render-protocol
  [p git-hash]
  (let [src-href (str repo-url "/blob/" git-hash "/" src-path "/"
                      (:file p) "#L" (:line p))
        doc-title (if (:doc p)
                    {:title  (clojure.string/replace (:doc p) #"\s+" " ")}
                    {})]
    (seq [[:span doc-title
           (:name p)]
          "&nbsp;"
          [:small "(line " [:a {:href src-href} (:line p)] ")" ]])))

(defn render-table
  [impl-objs protos git-hash]
  [:table.pure-table
   [:thead
    [:th]
    (for [impl-obj impl-objs
          :let [impl-name (-> impl-obj :name name)
                impl-doc (try (-> impl-obj :obj mp/meta-info :doc)
                              (catch IllegalArgumentException e nil))
                impl-title (if impl-doc
                             {:title (clojure.string/replace impl-doc
                                                             #"\s+" " ")}
                             {})]]
      [:th [:span impl-title
            impl-name]])]
   [:tbody
    (for [[i p] (map-indexed vector protos)]
      [:tr {:class (if (even? i) "pure-table-odd")}
       [:td (render-protocol p git-hash)]
       (for [impl-name (map :name impl-objs)]
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
        protos (extract-implementations (extract-protocols) impl-objs)
        git-hash (get-git-hash)
        header (render-header git-hash)
        table (render-table impl-objs protos git-hash)
        page (render-page header table)]
    (h/html5 page)))