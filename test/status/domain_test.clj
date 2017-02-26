(ns status.domain-test
  (:require [status.domain :as sut]
            [status.types :as type]
            [clojure.test :as t]))

(t/deftest test-error-checking
  (let [sys (sut/new-system)]
    (t/is (thrown? Exception (sut/capture sys 0 0))
          "Can't capture value for non-existing component")))

(t/deftest test-simple-meter
  (let [sys (sut/new-system)
        m (sut/create-component sys {::sut/name 'a ::sut/type type/TAny})
        m2 (sut/create-component sys {::sut/name 'a
                                      ::sut/type type/TAny
                                      ::sut/default-value 42})]
    (t/is (nil? (seq (sut/measurements sys m)))
          "New meter has no measurements")
    (t/is (nil? (sut/value sys m))
          "Signal of initial meter is undefined")

    (sut/capture sys m 1)
    (t/is (= 1 (count (sut/measurements sys m)))
          "Add one measurement gives a singleton list")
    (t/is (= 1 (sut/value sys m))
          "Add one measurement updates the value")

    (sut/capture sys m 2)
    (t/is (= 2 (count (sut/measurements sys m)))
          "Measurements are order in order of capture (choronologically)")
    (t/is (= 2 (sut/value sys m))
          "Signal shows last measurement as value")

    (t/is (= 42 (sut/value sys m2))
          "New component has default value when specified")))

(t/deftest test-min-max-signals
  (let [sys (sut/new-system)
        m1 (sut/create-component sys {::sut/name 'a ::sut/type type/TNumber})
        m2 (sut/create-component sys {::sut/name 'b ::sut/type type/TNumber})
        imin (sut/create-component
              sys
              {::sut/name 'min
               ::sut/function {::sut/function-id :min
                              ::sut/dependencies [m1 m2]
                              ::sut/type (type/fn-type (type/varargs-type [] type/TNumber) type/TNumber)}})
        imax (sut/create-component
              sys
              {::sut/name 'max
               ::sut/function {::sut/function-id :max
                              ::sut/dependencies [m1 m2]
                              ::sut/type (type/fn-type (type/varargs-type [] type/TNumber) type/TNumber)}})]

    (t/is (nil? (sut/value sys imin)))
    (t/is (nil? (sut/value sys imax)))

    (sut/capture sys m1 0)

    (t/is (nil? (sut/value sys imin)))
    (t/is (nil? (sut/value sys imax)))

    (sut/capture sys m2 1)

    (t/is (= 0 (sut/value sys imin)))
    (t/is (= 1 (sut/value sys imax)))

    (let [m3 (sut/create-component sys {::sut/name 'c ::sut/type type/TAny})]
      (t/is (thrown? Exception
                     (sut/create-component
                      sys
                      {::sut/name 'min
                       ::sut/function {::sut/function-id :min
                                       ::sut/dependencies [m3]
                                       ::sut/type (type/fn-type (type/varargs-type [] type/TNumber)
                                                                type/TNumber)}}))))))

(t/deftest test-weighted-signal
  (let [sys (sut/new-system)
        m1 (sut/create-component sys {::sut/name 'a ::sut/type type/TNumber})
        m2 (sut/create-component sys {::sut/name 'b ::sut/type type/TNumber})
        m3 (sut/create-component sys {::sut/name 'any ::sut/type type/TAny})
        w (sut/create-component
           sys
           {::sut/name 'w
            ::sut/function {::sut/function-id :weighted
                            ::sut/dependencies [m1 m2]
                            ::sut/parameters {::sut/weights [1 2]}
                            ::sut/type (type/fn-type (type/varargs-type [] type/TNumber) type/TNumber)}})]

    (t/is (nil? (sut/value sys w)))

    (sut/capture sys m1 0)
    (t/is (nil? (sut/value sys w)))

    (sut/capture sys m2 0)
    (t/is (= 0 (sut/value sys w)))

    (sut/capture sys m1 1)
    (sut/capture sys m2 2)
    (t/is (= 5 (sut/value sys w)))

    (t/is (thrown? Exception
                   (sut/create-component
                    sys
                    {::sut/name 'w
                     ::sut/function {::sut/function-id :weighted
                                     ::sut/dependencies [m1 m3]
                                     ::sut/parameters {::sut/weights [1 2]}
                                     ::sut/type (type/fn-type (type/varargs-type [] type/TNumber) type/TNumber)}}))
          "Weighted signal requires numeric inputs")

    (t/is (thrown? Exception (sut/create-component
                              sys
                              {::sut/name 'w
                               ::sut/function {::sut/function-id :weighted
                                               ::sut/dependencies [m1 m2]
                                               ::sut/parameters {::sut/weights [1]}
                                               ::sut/type (type/fn-type (type/varargs-type [] type/TNumber) type/TNumber)}}))
          "Weighted signal requires equal number of inputs and weights")))

(t/deftest test-merge-systems
  (let [sys (sut/new-system)
        _ (sut/create-component sys {::sut/name 'a ::sut/type type/TNumber})
        sysB (sut/new-system)
        m (sut/create-component sysB {::sut/name 'b ::sut/type type/TNumber})
        _ (sut/create-component
           sysB
           {::sut/name 'min
            ::sut/function {::sut/function-id :min
                            ::sut/dependencies [m]
                            ::sut/type (type/fn-type (type/varargs-type [] type/TNumber)
                                                     type/TNumber)}})]


    (sut/merge-systems sys sysB)
    (t/is (= 3 (count (sut/components sys))))
    (t/is (= 'b (sut/component-name (sut/get-component sys 1))))
    (t/is (= 'min (sut/component-name (sut/get-component sys 2))))
    (t/is (= #{1} (sut/dependencies (sut/get-component sys 2))))))


