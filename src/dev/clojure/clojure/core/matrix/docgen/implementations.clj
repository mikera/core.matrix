(ns clojure.core.matrix.docgen.implementations
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as mi]))

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
          (for [[name ns] mi/KNOWN-IMPLEMENTATIONS
                :when (not (#{:TODO} ns))]
            (try
              [ns (mi/get-canonical-object name)]
              (catch Throwable t nil)))))

(defn extract-implementations
  [protocols]
  (let [objs (get-impl-objs)]
    (for [p protocols
          :let [implementers (->> objs
                                  (filter #(extends? p %))
                                  (map type)
                                  (into #{}))]]
      (assoc p :implemented-by implementers))))
