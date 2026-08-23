(ns fress.test-runner
  (:require [clojure.test :refer [run-tests]]
            [fress.golden-test]))

(defn -main [& _args]
  (let [{:keys [fail error]} (run-tests 'fress.golden-test)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
