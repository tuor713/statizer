(ns status.core-test
  "End-to-end tests against the web service"
  (:require [clojure.test :as t]
            [status.core :as sut]
            [status.domain :as dom]
            [clj-http.client :as http]))

(def test-port "8080")

(defn- url [& elements]
  (apply str "http://localhost:" test-port elements))

(t/use-fixtures
  :each (fn [f] (f) (sut/clear!)))

(t/deftest test-signal-get
  (let [id (dom/id (sut/add-meter! 'a/signal))]
    (sut/capture! id 1)
    (let [resp (http/get (url "/signal/" id "/value"))]
      (t/is (= "1" (:body resp)))
      (t/is (= 200 (:status resp)))))

  (let [resp (http/get (url "/signal/" 12345 "/value")
                       {:throw-exceptions false})]
    (t/is (= 404 (:status resp))
          "Requesting a non-existing signal gives 404 status")))

(t/deftest test-signal-post-get
  (let [id (dom/id (sut/add-meter! 'a/signal))]
    (let [resp (http/post (url "/meter/" id "/value")
                          {:body "2"})]
      (t/is (= 200 (:status resp))))
    (let [resp (http/get (url "/signal/" id "/value"))]
      (t/is (= "2" (:body resp)))
      (t/is (= 200 (:status resp)))))

  (let [resp (http/post (url "/meter/" 12345 "/value")
                        {:body "2" :throw-exceptions false})]
    (t/is (= 404 (:status resp)))))


