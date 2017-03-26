(ns status.domain-test
  (:require [status.domain :as sut]
            [status.types :as type]
            [clojure.java.io :as io]
            [clojure.spec :as spec]
            [clojure.test :as t]

            [clojurewerkz.quartzite.scheduler :as sched]
            [clojurewerkz.quartzite.jobs :as job]))

(t/deftest test-error-checking
  (with-open [sys (sut/new-system)]
    (t/is (thrown? Exception (sut/capture sys 0 0))
          "Can't capture value for non-existing component")

    (t/is (thrown? Exception (sut/create-component sys {::sut/type type/TAny})))))

(t/deftest test-specs
  (t/is (spec/valid? ::sut/component
                     {::sut/id 0
                      ::sut/name 'a
                      ::sut/type type/TAny}))

  (t/testing "Source type"
    (t/is (spec/valid? ::sut/component
                       {::sut/id 0
                        ::sut/name 'a
                        ::sut/type type/TAny
                        ::sut/source {::sut/source-type ::sut/push}}))
    (t/is (not (spec/valid? ::sut/component
                            {::sut/id 0
                             ::sut/name 'a
                             ::sut/type type/TAny
                             ::sut/source {::sut/source-type ::sut/bad}})))))

(t/deftest test-simple-meter
  (with-open [sys (sut/new-system)]
    (let [m (sut/create-component sys {::sut/name 'a ::sut/type type/TAny})
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
            "New component has default value when specified")

      (let [c (sut/get-component sys m)]
        (t/is (not (sut/derived? c)))
        (t/is (= 'a (sut/component-name c)))))))

(t/deftest test-plain-keyword-support
  (with-open [sys (sut/new-system)]
    (let [m (sut/create-component sys {:name 'a :type type/TAny})])))

(t/deftest test-min-max-signals
  (with-open [sys (sut/new-system)]
    (let [m1 (sut/create-component sys {::sut/name 'a ::sut/type type/TNumber})
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
                                                                  type/TNumber)}})))))))

(t/deftest test-defaulting-policies
  (with-open [sys (sut/new-system)]
    (let [m1 (sut/create-component sys {:name 'a :type type/TNumber})
          m2 (sut/create-component sys {:name 'b :type type/TNumber})
          config {:name 'min
                  :function {:function-id :min
                             :dependencies [m1 m2]
                             :type ::type/number*->number}}
          min (sut/create-component sys config)
          min-with-default (sut/create-component sys (assoc config :default-value 0))
          min-ignore (sut/create-component sys (assoc-in
                                                config
                                                [:function :missing-policy]
                                                :ignore-value))
          min-default (sut/create-component
                       sys
                       (-> config
                           (assoc-in [:function :missing-policy] :default-input)
                           (assoc-in [:function :default-value] 0)))]
      (t/are [c v] (= (sut/value sys c) v)
        min nil
        min-with-default 0
        min-ignore nil
        min-default 0)

      (sut/capture sys m1 -1)
      (t/are [c v] (= (sut/value sys c) v)
        min nil
        min-with-default 0
        min-ignore -1
        min-default -1)

      (sut/capture sys m1 1)
      (t/is (= 1 (sut/value sys min-ignore)))
      (t/is (= 0 (sut/value sys min-default)))

      (sut/capture sys m2 2)
      (t/are [c v] (= (sut/value sys c) v)
        min 1
        min-with-default 1
        min-ignore 1
        min-default 1))))

(t/deftest test-updating-components
  (with-open [sys (sut/new-system)]
    (t/testing "Updating type of basic component"
      (let [m1 (sut/create-component sys {:name 'a :type type/TAny})
            min-cfg {:name 'min
                     :function {:function-id :min
                                :type ::type/number*->number
                                :dependencies [m1]}}]
        (t/is (thrown? Exception (sut/create-component sys min-cfg)))
        (sut/update-component sys m1 {:name 'a :type type/TNumber})
        (t/is (number? (sut/create-component sys min-cfg)))))

    (t/testing "Updating simple component resets the value"
      (let [m (sut/create-component sys {:name 'a :type type/TAny})]
        (sut/capture sys m 1)
        (t/is (= 1 (sut/value sys m)))
        (sut/update-component sys m {:name 'b :type type/TNumber})
        (t/is (nil? (sut/value sys m)))))

    (t/testing "Updating changes computed component function"
      (let [m1 (sut/create-component sys {:name 'a :type type/TNumber})
            m2 (sut/create-component sys {:name 'b :type type/TNumber})
            min (sut/create-component sys {:name 'f
                                           :function {:function-id :min
                                                      :type ::type/number*->number
                                                      :dependencies [m1 m2]}})]
        (sut/capture sys m1 0)
        (sut/capture sys m2 1)
        (t/is (= 0 (sut/value sys min)))

        (sut/update-component sys min {:name 'f
                                       :function {:function-id :max
                                                  :type ::type/number*->number
                                                  :dependencies [m1 m2]}})
        (t/is (= 1 (sut/value sys min)))))

    (t/testing "Updating from pull to push removes scheduling"
      (let [tf (io/file ".status.test")]
        (try
          (spit tf "1")
          (let [c (sut/create-component
                   sys
                   {::sut/name 'pull
                    ::sut/type type/TAny
                    ::sut/source {::sut/source-type ::sut/url-source
                                  ::sut/url (.toURL tf)
                                  ::sut/schedule {::sut/schedule-type ::sut/simple
                                                  ::sut/interval-in-ms 100}}})]
            (t/is (not (nil? (sched/get-job (:scheduler sys) (job/key (str "job." c))))))
            (t/is (= "1" (sut/value sys c)))
            (sut/update-component sys c {:name 'push :type type/TAny})
            (t/is (nil? (sched/get-job (:scheduler sys) (job/key (str "job." c))))))
          (finally
            (.delete tf)))))))

(t/deftest test-delete-component
  (with-open [sys (sut/new-system)]
    (t/testing "Basic delete"
      (let [id (sut/create-component sys {:name 'a :type type/TAny})]
        (sut/capture sys id 1)
        (t/is (contains? (set (map sut/id (sut/components sys))) id))
        (t/is (= 1 (sut/value sys id)))

        (sut/delete-component sys id)
        (t/is (not (contains? (set (map sut/id (sut/components sys))) id)))
        (t/is (thrown? IllegalArgumentException (sut/value sys id)))))

    (t/testing "Deleting with dependencies"
      (let [id (sut/create-component sys {:name 'a :type type/TNumber})
            min (sut/create-component sys {:name 'min
                                           :function {:function-id :min
                                                      :type ::type/number*->number
                                                      :dependencies [id]}})]
        (t/is (thrown? IllegalStateException
                       (sut/delete-component sys id)))
        (t/is (contains? (set (map sut/id (sut/components sys))) id))
        (sut/delete-component sys min)
        (t/is (not (contains? (set (map sut/id (sut/components sys))) min)))
        (sut/delete-component sys id)
        (t/is (not (contains? (set (map sut/id (sut/components sys))) id)))
        ))))

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

(t/deftest test-pull-signal
  (with-open [sys (sut/new-system)]
    (let [tf (io/file ".status.test")]
      (try
        (spit tf "1")
        (let [c (sut/create-component
                 sys
                 {::sut/name 'pull
                  ::sut/type type/TAny
                  ::sut/source {::sut/source-type ::sut/url-source
                                ::sut/url (.toURL tf)
                                ::sut/schedule {::sut/schedule-type ::sut/simple
                                                ::sut/interval-in-ms 100}}})]
          (t/is (= "1" (sut/value sys c)))
          (spit tf "2")
          (t/is (= "1" (sut/value sys c))
                "No immediate update")
          (Thread/sleep 150)
          (t/is (= "2" (sut/value sys c))))
        (finally
          (.delete tf))))))

(t/deftest test-merge-systems
  (with-open [sys (sut/new-system)]
    (let [_ (sut/create-component sys {::sut/name 'a ::sut/type type/TNumber})
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
      (t/is (= #{1} (sut/dependencies (sut/get-component sys 2)))))))

(t/deftest test-multi-value-components
  (with-open [sys (sut/new-system)]
    (let [mid (sut/create-component
               sys
               {::sut/name 'map
                ::sut/type (type/map-type type/TAny type/TNumber)})]
      (t/is (nil? (sut/value sys mid)))

      (sut/capture sys mid {:a 1 :b 2})
      (t/is (= {:a 1 :b 2}
               (sut/value sys mid)))

      (sut/capture sys mid [:c 3])
      (t/is (= {:a 1 :b 2 :c 3}
               (sut/value sys mid))))))
