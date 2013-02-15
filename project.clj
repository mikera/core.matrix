(defproject net.mikera/core.matrix "0.0.13-SNAPSHOT"
  :url "https://github.com/mikera/matrix-api"
  :license {:name "Eclipse Public License (EPL)"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src/main"]
  :java-source-paths ["src/test/java"]
  :test-paths ["src/test" "src/test"]
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :profiles {:dev {:dependencies [[com.google.caliper/caliper "0.5-rc1"]
                                  [net.mikera/cljunit "0.1.7"]
                                  [criterium/criterium "0.3.1"]]}})
