(ns status.types-test
  (:require [status.types :as sut]
            [clojure.test :as t]))

(defn equiv? [ta tb]
  (and (sut/substitutes? ta tb)
       (sut/substitutes? tb ta)))

(defn below? [ta tb]
  (and (sut/substitutes? ta tb)
       (not (sut/substitutes? tb ta))))

(defn incompatible? [ta tb]
  (and (not (sut/substitutes? ta tb))
       (not (sut/substitutes? tb ta))))

(t/deftest test-any
  (t/is (sut/substitutes? sut/TAny sut/TAny)
        "Any < Any")
  (t/is (sut/substitutes? sut/TAny sut/TNumber)
        "Any < Number")
  (t/is (sut/substitutes? sut/TAny (sut/fn-type (sut/tuple-type) sut/TAny))
        "Any < function"))

(t/deftest test-string-type
  (t/is (below? sut/TAny sut/TString))
  (t/is (incompatible? sut/TNumber sut/TString)))

(t/deftest test-numeric-types
  (t/is (below? sut/TAny sut/TNumber))

  (t/testing "Range types"
    (t/is (below? sut/TNumber (sut/range-type 0 1)))

    (t/is (below? (sut/range-type 0 3) (sut/range-type 1 2)))
    (t/is (incompatible? (sut/range-type 0 2)
                         (sut/range-type 1 3)))

    (t/is (below? (sut/range-type sut/TNumber 0 1 true true)
                  (sut/range-type sut/TNumber 0 1 false true)))
    (t/is (incompatible? (sut/range-type sut/TNumber 0 1 true false)
                         (sut/range-type sut/TNumber 0 1 false true))))

  (t/testing "Indicator type"
    (t/is (sut/substitutes? sut/TNumber sut/TIndicator))
    (t/is (not (sut/substitutes? sut/TIndicator sut/TNumber)))
    (t/is (equiv? (sut/range-type 0 1) sut/TIndicator))))

(t/deftest test-vector-type
  (t/is (below? sut/TAny (sut/vector-type sut/TAny)))
  (t/is (sut/substitutes? (sut/vector-type sut/TAny) (sut/vector-type sut/TAny)))
  (t/is (below? (sut/vector-type sut/TAny) (sut/vector-type sut/TNumber))))

(t/deftest test-tuple-types
  (t/is (below? sut/TAny (sut/tuple-type sut/TAny sut/TAny)))
  (t/is (below? (sut/tuple-type sut/TAny sut/TAny) (sut/tuple-type sut/TAny sut/TNumber)))
  (t/is (not (sut/substitutes? (sut/tuple-type sut/TNumber sut/TAny) (sut/tuple-type sut/TAny sut/TNumber))))
  (t/is (incompatible? (sut/tuple-type sut/TAny) (sut/tuple-type sut/TAny sut/TAny)))
  (t/is (incompatible? (sut/tuple-type sut/TAny) sut/TNumber))
  (t/is (incompatible? (sut/tuple-type sut/TAny) (sut/range-type 0 1)))

  (t/testing "Varargs"
    (t/is (below? (sut/varargs-type [sut/TAny sut/TAny] sut/TAny)
                  (sut/tuple-type sut/TAny sut/TAny)))
    (t/is (below? (sut/varargs-type [sut/TAny sut/TAny] sut/TAny)
                  (sut/tuple-type sut/TAny sut/TAny sut/TNumber)))
    (t/is (below? (sut/varargs-type [sut/TAny sut/TAny] sut/TAny)
                  (sut/varargs-type [sut/TAny sut/TAny] sut/TNumber)))
    (t/is (below? (sut/varargs-type [sut/TAny sut/TAny] sut/TAny)
                  (sut/varargs-type [sut/TAny sut/TAny sut/TAny] sut/TAny)))
    (t/is (incompatible? sut/TNumber (sut/varargs-type [sut/TAny sut/TAny] sut/TAny)))))

(t/deftest test-map-types
  (t/is (below? sut/TAny (sut/map-type sut/TAny sut/TAny)))
  (t/is (incompatible? sut/TNumber (sut/map-type sut/TAny sut/TAny)))
  (t/is (below? (sut/map-type sut/TAny sut/TAny)
                (sut/map-type sut/TNumber sut/TAny)))
  (t/is (below? (sut/map-type sut/TAny sut/TAny)
                (sut/map-type sut/TAny sut/TNumber)))
  (t/is (incompatible? (sut/map-type sut/TNumber sut/TAny)
                       (sut/map-type sut/TAny sut/TNumber)))
  (t/is (not (sut/map-type? sut/TAny)))
  (t/is (sut/map-type? (sut/map-type sut/TAny sut/TAny)))

  )

(t/deftest test-function-types
  (t/is (below? sut/TAny (sut/fn-type (sut/tuple-type) sut/TAny)))

  (t/is (thrown? IllegalArgumentException
                 (sut/fn-type sut/TAny sut/TAny)))

  (t/is (below? (sut/fn-type (sut/tuple-type) sut/TAny)
                (sut/fn-type (sut/tuple-type) sut/TNumber)))
  (t/is (below? (sut/fn-type (sut/tuple-type sut/TNumber) sut/TAny)
                (sut/fn-type (sut/tuple-type sut/TAny) sut/TAny)))

  (t/is (incompatible? (sut/fn-type (sut/tuple-type sut/TAny) sut/TAny)
                       (sut/fn-type (sut/tuple-type sut/TAny sut/TAny) sut/TAny)))
  (t/is (incompatible? (sut/fn-type (sut/tuple-type sut/TAny) sut/TAny) sut/TNumber))

  (t/is (equiv? sut/TAny (sut/fn-range (sut/fn-type (sut/tuple-type sut/TAny) sut/TAny))))
  (t/is (equiv? (sut/tuple-type sut/TAny)
                (sut/fn-domain (sut/fn-type (sut/tuple-type sut/TAny) sut/TAny))))

  (t/is (sut/fn-applicable? (sut/fn-type (sut/tuple-type sut/TAny sut/TAny) sut/TAny)
                            (sut/tuple-type sut/TAny sut/TAny)))
  (t/is (sut/fn-applicable? (sut/fn-type (sut/tuple-type sut/TAny sut/TAny) sut/TAny)
                            (sut/tuple-type sut/TAny sut/TNumber)))
  (t/is (not (sut/fn-applicable? (sut/fn-type (sut/tuple-type sut/TNumber sut/TAny) sut/TAny)
                                 (sut/tuple-type sut/TAny sut/TAny))))
  (t/is (not (sut/fn-applicable? (sut/fn-type (sut/tuple-type sut/TAny sut/TAny) sut/TAny)
                                 (sut/tuple-type sut/TAny)))))

(t/deftest test-aliases
  (t/is (equiv? ::sut/number*->number
                (sut/fn-type (sut/varargs-type [] sut/TNumber) sut/TNumber)))
  (t/is (equiv? ::sut/indicator*->indicator
                (sut/fn-type (sut/varargs-type [] sut/TIndicator) sut/TIndicator)))
  (t/is (= sut/TNumber
           (sut/fn-range ::sut/number*->number)))
  (t/is (equiv? (sut/varargs-type [] sut/TNumber)
                (sut/fn-domain ::sut/number*->number)))

  (t/is (equiv? ::sut/multi-indicator (sut/map-type sut/TString sut/TIndicator)))
  (t/is (sut/map-type? ::sut/multi-indicator)))
