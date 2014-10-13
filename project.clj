;; This project.clj is provided as a convenience for Leiningen users
;;
;; The official core.matrix project configuration is in the pom.xml
;; dependencies / configuration in this file may be out of date
;; if in doubt, please refer to the latest pom.xml

(defproject net.mikera/core.matrix "0.30.3-SNAPSHOT"
  :url "https://github.com/mikera/matrix-api"
  :license {:name "Eclipse Public License (EPL)"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  
  :source-paths ["src/main/clojure"]
  :java-source-paths ["src/test/java"]
  
  :test-paths ["src/test/clojure" "src/test/java"]
  
  :dependencies [[org.clojure/clojure "1.6.0"]]
  
  :marginalia {:javascript ["http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"]}
  
  :profiles {:dev {:dependencies [[net.mikera/cljunit "0.3.1"]
                                  [com.google.caliper/caliper "0.5-rc1"]
                                  [criterium/criterium "0.4.3"]
                                  [org.clojure/tools.macro "0.1.5"]
                                  [hiccup "1.0.5"]
                                  [net.mikera/vectorz-clj "0.26.1"]
                                  [org.clojure/test.check "0.5.9"]]
                   :source-paths ["src/main/clojure" "src/dev/clojure"]
                   :jvm-opts ^:replace []}})
