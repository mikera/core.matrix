(ns clojure.core.matrix.impl.common
  (:require [clojure.string :as s]
            [clojure.core.matrix.utils :as u]
            [clojure.core.matrix.implementations :as mi]
            [clojure.java.shell :refer [sh]]))

(defn get-impl-objs
  "Returns a list of available implementations' objects"
  []
  (filter second
          (for [[name ns] mi/KNOWN-IMPLEMENTATIONS
                :when (not (#{:TODO :persistent-vector} ns))]
            (try
              {:name name, :obj (mi/get-canonical-object name)}
              (catch Throwable t nil)))))

(defn find-implementers
  "Returns a set of implementation names of implementations that
   support provided protocol"
  [protocol impl-objs]
  (->> impl-objs
       (filter #(->> % :obj class (u/extends-deep? protocol)))
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
      (s/replace #"'" "")))
