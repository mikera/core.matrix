(defproject net.mikera/core.matrix "0.16.1-SNAPSHOT"
  :description "mars0i's fork of mikera/core.matrix"
  :url "https://github.com/mars0i/core.matrix"
  :license {:name "Eclipse Public License (EPL)"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src/main/clojure"]
  :java-source-paths ["src/test/java"]
  :test-paths ["src/test/clojure" "src/test/java"]
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :marginalia {:javascript ["http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"]}
  :profiles {:dev {:dependencies [[com.google.caliper/caliper "0.5-rc1"]
                                  [net.mikera/cljunit "0.3.0"]
                                  [criterium/criterium "0.4.2"]
                                  [hiccup "1.0.4"]
                                  ;[net.mikera/vectorz-clj "0.16.0"]
                                  [mars0i/vectorz-clj "0.17.0-SNAPSHOT"]
                                  [slingshot "0.10.3"]
                                  [org.jblas/jblas "1.2.3"]
                                  [clatrix/clatrix "0.3.0"]
                                  [reiddraper/simple-check "0.4.1"]
                                  [org.clojure/tools.macro "0.1.5"]
                                  [com.google.caliper/caliper "0.5-rc1"]]
                   :source-paths ["src/main/clojure" "src/dev/clojure"]
                   :jvm-opts ^:replace []}})
