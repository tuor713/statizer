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

    (types/map-type types/TAny types/TNumber)

    (types/fn-type (types/tuple-type types/TNumber) types/TAny)))

(t/deftest test-file-persistence
  (let [sys (dom/new-system)]
    (t/is (= @(:config-ref sys) @(:config-ref (fs-load (fs-save sys)))))

    (let [m1 (dom/create-component sys {::dom/name 'a.job ::dom/type types/TAny})]
      (t/is (= @(:config-ref sys) @(:config-ref (fs-load (fs-save sys)))))

      (dom/capture sys m1 0)
      (t/is (= @(:config-ref sys) @(:config-ref (fs-load (fs-save sys))))
            "Measurements are not saved as part of the config")

      (dom/create-component sys {::dom/name 'b.job ::dom/type types/TIndicator})
      (t/is (= @(:config-ref sys) @(:config-ref (fs-load (fs-save sys))))
            "Serialize indicator (range) type"))

    (let [sys (dom/new-system)
          m1 (dom/create-component sys {::dom/name 'b.job ::dom/type types/TIndicator})
          _ (dom/create-component
             sys
             {::dom/name 'min
              ::dom/function {::dom/function-id :min
                              ::dom/dependencies [m1]
                              ::dom/type (types/fn-type (types/varargs-type [] types/TNumber)
                                                        types/TNumber)}})
          _ (dom/create-component
             sys
             {::dom/name 'max
              ::dom/function {::dom/function-id :max
                              ::dom/dependencies [m1]
                              ::dom/type (types/fn-type (types/varargs-type [] types/TNumber)
                                                        types/TNumber)}})]
      (t/is (= 3 (count (dom/components (fs-load (fs-save sys)))))
            "Serialize min & max computed signal"))

    (let [sys (dom/new-system)
          m1 (dom/create-component sys {::dom/name 'm ::dom/type types/TIndicator})
          w (dom/create-component
             sys
             {::dom/name 'w
              ::dom/function {::dom/function-id :weighted
                              ::dom/dependencies [m1]
                              ::dom/parameters {::dom/weights [2]}
                              ::dom/type (types/fn-type (types/varargs-type [] types/TNumber)
                                                        types/TNumber)}})
          sysB (fs-load (fs-save sys))]
      (t/is (= 2 (count (dom/components sysB))))
      (dom/capture sysB m1 1)

      (t/is (nil? (dom/value sys w)))
      (t/is (= 2 (dom/value sysB w))))))

