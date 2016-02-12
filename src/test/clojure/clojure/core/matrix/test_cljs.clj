(ns clojure.core.matrix.test-cljs
  (:require [clojure.java.io :as io]
            [cljs.build.api])
  (:import javax.script.ScriptEngineManager))

(def CONSOLE-HACK "
var global = this;
var console = {};
console.debug = print;
console.warn = print;
console.log = print;
")

(def TEST-PATH "resources/public/js/unit-test.js")

(defn compile-cljs
  []
  (cljs.build.api/build "src" {:output-to TEST-PATH
                               :asset-path   "js/out"
                               :optimizations :advanced
                               :parallel-build true
                               :pretty-print false}))



(defn eval-cljs
  "Run Clojurescript in a Nashorn engine."
  [cljs]
  (let [engine (.getEngineByName (ScriptEngineManager.) "Nashorn")]
    (.eval engine CONSOLE-HACK)
    (.eval engine (io/reader cljs))))

(defn run-tests
  []
  (eval-cljs TEST-PATH))

(defn build-and-run-tests
  []
  (compile-cljs)
  (run-tests))
