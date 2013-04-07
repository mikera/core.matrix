(defproject net.mikera/core.matrix "0.5.0"
  :url "https://github.com/mikera/matrix-api"
  :license {:name "Eclipse Public License (EPL)"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src/main/clojure"]
  :java-source-paths ["src/test/java"]
  :test-paths ["src/test/clojure" "src/test/java"]
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :profiles {:dev {:dependencies [[com.google.caliper/caliper "0.5-rc1"]
                                  [net.mikera/cljunit "0.2.0"]
                                  [criterium/criterium "0.3.1"]]}})
