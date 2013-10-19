(ns clojure.core.matrix.docgen.implementations
  (:require [clojure.string :as s]
            [hiccup.page :as h]
            [clojure.core.matrix.utils :as utils]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as mi]
            [clojure.core.matrix.docgen.common :as c]))

;; ## Info
;; This file provides rather hacky solution for generating
;; html documentation about various matrix implementations available in
;; core.matrix

(def repo-url "https://github.com/mikera/core.matrix")
(def src-path "src/main/clojure")

(defn render-header
  [git-hash]
  (seq [[:h2 "Protocol/Implementation summary"]
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
                    {:title  (s/replace (:doc p) #"\s+" " ")}
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
                             {:title (s/replace impl-doc #"\s+" " ")}
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
  (let [impl-objs (c/get-impl-objs)
        protos (c/extract-implementations (utils/extract-protocols) impl-objs)
        git-hash (c/get-git-hash)
        header (render-header git-hash)
        table (render-table impl-objs protos git-hash)]
    (render-page header table)))