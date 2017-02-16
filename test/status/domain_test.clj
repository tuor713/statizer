(ns status.domain-test
  (:require [status.domain :as sut]
            [clojure.test :as t]))

(t/deftest test-simple-meter
  (let [sys (sut/new-system)
        [m _] (sut/make-meter sys 'a/signal)
        m2 (sut/capture m 1)
        m3 (sut/capture m2 2)]
    (t/is (= (:measurements m) [])
          "New meter has no measurements")
    (t/is (= (:measurements m2) [1])
          "Add one measurement gives a singleton list")
    (t/is (nil? (sut/value m nil))
          "Signal of initial meter is undefined")
    (t/is (= (sut/value m2 nil) 1)
          "Add one measurement updates the value")
    (t/is (= (:measurements m3) [1 2])
          "Measurements are order in order of capture (choronologically)")
    (t/is (= (sut/value m3 nil) 2)
          "Signal shows last measurement as value")))

(t/deftest test-aggregate-signal
  (let [sys (sut/new-system)
        [m s1] (sut/make-meter sys 'signal)
        [i s2] (sut/make-computed-signal s1 'indicator [(sut/id m)] #(* 2 %))
        s3 (sut/sys-capture s2 (sut/id m) 1)]
    (t/is (nil? (sut/value i s2)))
    (t/is (= 2 (sut/value i s3))))

  (t/is (thrown? Exception (sut/make-computed-signal
                            (sut/new-system)
                            'indicator
                            [0]
                            inc))
        "Exception is triggered for creating a computed signal with inputs that don't exist"))

(t/deftest test-predefined-signals
  (let [sys (sut/new-system)
        [m1 s1] (sut/make-meter sys 'a.signal)
        [m2 s2] (sut/make-meter s1 'b.siginal)
        [imin s3] (sut/make-min-signal s2 'min [(sut/id m1) (sut/id m2)])
        [imax s4] (sut/make-max-signal s3 'max [(sut/id m1) (sut/id m2)])]

    (t/is (nil? (sut/value imin s4)))
    (t/is (nil? (sut/value imax s4)))

    (t/is (nil? (sut/value imin (sut/sys-capture s4 (sut/id m1) 0))))
    (t/is (nil? (sut/value imax (sut/sys-capture s4 (sut/id m1) 0))))

    (t/is (= 0 (sut/value imin (sut/sys-capture (sut/sys-capture s4 (sut/id m1) 0)
                                                (sut/id m2)
                                                1))))
    (t/is (= 1 (sut/value imax (sut/sys-capture (sut/sys-capture s4 (sut/id m1) 0)
                                                (sut/id m2)
                                                1))))))

