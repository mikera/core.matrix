(ns test.misc.loading-test)

(defonce loaded (atom nil))

(Thread/sleep 10)

(reset! loaded true)

