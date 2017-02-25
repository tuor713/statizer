(ns status.persistence-test
  (:require [status.persistence :as sut]
            [status.domain :as dom]
            [status.types :as types]
            [clojure.java.io :as io]
            [clojure.test :as t]))

(defn- fs-save [sys]
  (let [bf (java.io.ByteArrayOutputStream.)]
    (sut/save-system (sut/file-persistence bf) sys)
    (.toByteArray bf)))

(defn- fs-load [bytes]
  (sut/load-system (sut/file-persistence (java.io.ByteArrayInputStream. bytes))))

(t/deftest test-type-edn-persistence
  (t/are [t] (= t (sut/edn-read-string (pr-str t)))
    types/TAny
    types/TNumber
    types/TIndicator

    (types/vector-type types/TAny)

    (types/tuple-type)
    (types/tuple-type types/TNumber)
    (types/tuple-type types/TAny types/TNumber)

    (types/varargs-type [types/TNumber types/TNumber] types/TAny)

    (types/fn-type (types/tuple-type types/TNumber) types/TAny)))

(t/deftest test-file-persistence
  (let [sys (dom/new-system)
        [m1 s1] (dom/make-meter sys 'a.job types/TAny)
        s1-with-measurement (dom/sys-capture s1 (dom/id m1) 0)]

    (t/is (= sys (fs-load (fs-save sys))))
    (t/is (= s1 (fs-load (fs-save s1))))
    (t/is (= s1 (fs-load (fs-save s1-with-measurement)))
          "Measurements are not saved as part of the config")

    (let [s2 (second (dom/make-meter sys 'a.job types/TIndicator))]
      (t/is (= s2 (fs-load (fs-save s2)))
            "Serialize indicator (range) type"))

    (let [[m1 s1] (dom/make-meter sys 'a.job types/TIndicator)
          [_ s2] (dom/make-min-signal s1 'min.jobs [(dom/id m1)])
          [_ s3] (dom/make-max-signal s1 'min.jobs [(dom/id m1)])]
      (t/is (= 2 (count (dom/components (fs-load (fs-save s2)))))
            "Serialize min computed signal")
      (t/is (= 2 (count (dom/components (fs-load (fs-save s3)))))
            "Serialize max computed signal"))

    (let [[m1 s1] (dom/make-meter sys 'a types/TIndicator)
          [m2 s2] (dom/make-weighted-signal s1 'w [(dom/id m1)] [2])
          s3 (fs-load (fs-save s2))
          s4 (dom/sys-capture s3 (dom/id m1) 1)]
      (t/is (= 2 (count (dom/components s4))))
      (let [s5 (dom/sys-capture s4 (dom/id m1) 1)]
        (t/is (= 2 (dom/value (dom/get-component s5 (dom/id m2)) s5)))))))

