(ns status.core-test
  "End-to-end tests against the web service"
  (:require [clojure.test :as t]
            [status.core :as sut]
            [status.domain :as dom]
            [status.types :as type]
            [clj-http.client :as http]
            [cheshire.core :as json]
            [status.types :as type]))

(def test-port "8080")

(defn- url [& elements]
  (apply str "http://localhost:" test-port elements))

(t/use-fixtures
  :each (fn [f] (f) (sut/clear!)))

(defn- ok? [resp] (= 200 (:status resp)))
(defn- not-found? [resp] (= 404 (:status resp)))

(defn- returns? [expected resp]
  (and (ok? resp)
       (= (:body resp) expected)))

(defn- json? [expected resp]
  (and (ok? resp)
       (= expected (json/parse-string (:body resp) true))))

(defn get-signal [id]
  (http/get (url "/api/signal/" id)
            {:throw-exceptions false}))

(defn get-signal-value [id]
  (http/get (url "/api/signal/" id "/value")
            {:throw-exceptions false}))

(defn post-value [id value]
  (http/post (url "/api/meter/" id "/value")
             {:body (pr-str value)
              :throw-exceptions false}))

(t/deftest test-signal-get
  (let [id (dom/id (sut/add-meter! 'a/signal))]
    (sut/capture! id 1)
    (t/is (returns? "1" (get-signal-value id)))

    (t/is (json? {:value 1 :name "a/signal" :id 0 :dependencies []} (get-signal id))))

  (t/is (not-found? (get-signal-value 12345))
        "Requesting a non-existing signal gives 404 status"))

(t/deftest test-signal-post-get
  (let [id (dom/id (sut/add-meter! 'a/signal))]
    (t/is (ok? (post-value id 2)))
    (t/is (returns? "2" (get-signal-value id))))

  (t/is (not-found? (post-value 12345 2))))

(t/deftest test-min-max-get
  (let [a-id (dom/id (sut/add-meter! 'a.signal type/TNumber))
        b-id (dom/id (sut/add-meter! 'b.signal type/TNumber))
        c-id (dom/id (sut/add-min-signal! 'min [a-id b-id]))
        d-id (dom/id (sut/add-max-signal! 'max [a-id b-id]))]
    (t/is (ok? (post-value a-id 0)))
    (t/is (ok? (post-value b-id 1)))
    (t/is (returns? "0" (get-signal-value c-id)))
    (t/is (returns? "1" (get-signal-value d-id)))))




