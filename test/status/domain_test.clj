(ns status.domain-test
  (:require [status.domain :as sut]
            [clojure.test :as t]))

(t/deftest test-simple-meter
  (let [m (sut/make-meter 1 'a/signal)
        m2 (sut/capture m 1)
        m3 (sut/capture m2 2)]
    (t/is (= (:measurements m) [])
          "New meter has no measurements")
    (t/is (= (:measurements m2) [1])
          "Add one measurement gives a singleton list")
    (t/is (nil? (sut/value m))
          "Signal of initial meter is undefined")
    (t/is (= (sut/value m2) 1)
          "Add one measurement updates the value")
    (t/is (= (:measurements m3) [1 2])
          "Measurements are order in order of capture (choronologically)")
    (t/is (= (sut/value m3) 2)
          "Signal shows last measurement as value")))




