(ns clojure.core.matrix.docgen.common
  (:use [clojure.java.shell :only [sh]])
  (:require [clojure.reflect :as r]
            [clojure.string :as s]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as imp]
            [clojure.core.matrix.multimethods :as mm]
            [clojure.core.matrix.implementations :as mi]
            [criterium.core :as cr]))

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
      (s/replace #"'" "")))