(defproject net.mikera/core.matrix "0.50.0-SNAPSHOT"
  :url "https://github.com/mikera/core.matrix"
  :license {:name "Eclipse Public License (EPL)"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0"]]

  :source-paths      ["src/main/clojure"]
  :java-source-paths ["src/main/java"]
  :test-paths        ["src/test/clojure" "src/test/java"]

  :profiles {:dev {:dependencies
                   [[net.mikera/vectorz-clj "0.43.1-SNAPSHOT" :exclusions [net.mikera/core.matrix]]
                    [clatrix "0.5.0" :exclusions [net.mikera/core.matrix]]

                    [org.clojure/test.check "0.8.2"]
                    [net.mikera/cljunit "0.3.1"]
                    [criterium/criterium "0.4.3"]
                    [org.clojure/tools.macro "0.1.5"]
                    [hiccup "1.0.5"]]

                   :plugins [[lein-codox "0.9.0"]]
                   :source-paths ["src/dev/clojure"]
                   :java-source-paths  ["src/test/java"]
                   :jvm-opts ^:replace []}

             :test {:dependencies [[net.mikera/cljunit "0.3.1"]
                                   [criterium/criterium "0.4.3"]
                                   [org.clojure/tools.macro "0.1.5"]
                                   [clatrix "0.5.0"]
                                   [net.mikera/vectorz-clj "0.43.1-SNAPSHOT"]
                                   [org.clojure/test.check "0.8.2"]]}

             :cljs-dev {:dependencies [[org.clojure/clojurescript "1.7.228"]
                                       [thinktopic/aljabr "0.1.0-SNAPSHOT" :exclusions [net.mikera/core.matrix]]]

                    :plugins [[lein-figwheel "0.5.0-6"]
                              [lein-cljsbuild "1.1.2"]]

                    :cljsbuild {:builds
                                [{:id :dev
                                  :figwheel true
                                  :source-paths ["src/main/clojure" "src/test/cljs" "src/test/clojure"
                                                 "checkouts/aljabr/src"]
                                  :compiler {:output-to "resources/public/js/unit-test.js"
                                             :asset-path   "js/out"
                                             :main "clojure.core.matrix.test-basics"
                                             :optimizations :none
                                             :parallel-build true
                                             :pretty-print true}}]

                                ;:test-commands {"unit" ["phantomjs" "resources/public/js/unit-test.js"]}
                                }

                    :figwheel {:load-warninged-code true
                               :css-dirs ["resources/public/css"]
                               :server-port 8765}}

             :cljs {:dependencies [[org.clojure/clojurescript "1.7.228"]]
                    :plugins [[lein-cljsbuild "1.1.2"]]

                    :cljsbuild {:builds
                                [{:id :prod
                                  :jar true
                                  :source-paths ["src/main/clojure"]
                                  :compiler {:output-to "resources/public/js/core.matrix.js"
                                             :optimizations :advanced
                                             :pretty-print false}}]}}
             }

  :marginalia {:javascript ["http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"]}


  :codox {:namespaces [clojure.core.matrix
                       clojure.core.matrix.dataset
                       clojure.core.matrix.io
                       clojure.core.matrix.linear
                       clojure.core.matrix.random
                       clojure.core.matrix.operators
                       clojure.core.matrix.protocols
                       clojure.core.matrix.random
                       clojure.core.matrix.implementations
                       clojure.core.matrix.select
                       clojure.core.matrix.stats]
          :src-dir-uri "https://github.com/mikera/core.matrix/blob/master/"
          :src-linenum-anchor-prefix "L"})

