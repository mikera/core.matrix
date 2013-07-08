(ns clojure.core.matrix.docgen.implementations
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as mi]
            [hiccup.page :as h]))

;; ## Info
;; This file provides rather hacky solution for generating
;; html documentation about various matrix implementations available in
;; core.matrix

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
          (for [;;[name ns] mi/KNOWN-IMPLEMENTATIONS
                [name ns] {:ndarray 'clojure.core.matrix.impl.ndarray}
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

(defn generate
  []
  (let [impl-objs (get-impl-objs)
        impl-names (map :name impl-objs)
        protos (extract-implementations (extract-protocols) impl-objs)]
    (prn impl-names)
    (h/html5
     [:head
      [:title "Protocol/Implementation summary"]]
     [:body
      [:table
       [:tr
        [:th "Protocol"]
        (for [impl-name impl-names] [:th (name impl-name)])]
       (for [p protos]
         [:tr
          [:td (:name p)]
          (for [impl-name impl-names]
            [:td (if ((:implemented-by p) impl-name) "+" "-")])
          ])]
      ])))