(ns status.core
  (:require [clojure.edn :as edn]
            [clojure.walk :as walk]

            [io.pedestal.http :as bootstrap]
            [io.pedestal.http.route :as route]
            [io.pedestal.http.body-params :as body-params]
            [io.pedestal.http.route.definition :refer [defroutes]]

            [ring.util.request :as ring-req]
            [ring.util.response :as ring-resp]

            [hiccup.core :as html]

            [status.types :as type]
            [status.domain :as dom]
            [status.persistence :as persist]
            [cheshire.core :as json]))

;; Commands & Queries

(defonce state (atom (dom/new-system)))
(def persistence (atom nil))

(defn save! []
  (when @persistence
    (persist/save-system @persistence @state)))

(defn load! []
  (when @persistence
    (let [sys (persist/load-system @persistence)]
      (reset! state sys))))

(defn clear! []
  (reset! state (dom/new-system))
  (future (save!)))

(defn- update! [f]
  (let [result (f @state)]
    (future (save!))
    result))

(defn add-meter!
  ([name] (add-meter! name type/TAny))
  ([name type] (update! #(dom/create-component % {::dom/name name
                                                         ::dom/type type}))))

(defn add-min-signal! [name inputs]
  (update! #(dom/create-component
                    %
                    {::dom/name name
                     ::dom/function {::dom/function-id :min
                                     ::dom/dependencies inputs
                                     ::dom/type ::type/number*->number}})))

(defn add-max-signal! [name inputs]
  (update! #(dom/create-component
                    %
                    {::dom/name name
                     ::dom/function {::dom/function-id :max
                                     ::dom/dependencies inputs
                                     ::dom/type ::type/number*->number}})))

(defn add-weighted-signal! [name inputs weights]
  (update! #(dom/create-component
                    %
                    {::dom/name name
                     ::dom/function {::dom/function-id :weighted
                                     ::dom/dependencies inputs
                                     ::dom/parameters {::dom/weights weights}
                                     ::dom/type ::type/number*->number}})))

(defn capture! [id value]
  (dom/capture @state id value))

;; Web handlers

(defn format-response [req data]
  (let [format (get-in req [:query-params :format])
        formatter
        (cond
          (= "edn" format) bootstrap/edn-response
          :else bootstrap/json-response)]
    (formatter data)))

(defn all-signals [req]
  (let [ss @state]
    (format-response
     req
     (vec
      (sort-by :name
               (for [comp (dom/components ss)]
                 {:id (dom/id comp)
                  :name (dom/component-name comp)
                  :value (dom/value ss (dom/id comp))}))))))

(defn get-signal
  [req]
  (if (= "all" (get-in req [:path-params :id]))
    (all-signals req)
    (let [id (Long/parseLong (get-in req [:path-params :id]))]
      (if-let [signal (dom/get-component @state id)]
        (format-response
         req
         {:id id
          :name (dom/component-name signal)
          :value (dom/value @state id)
          :dependencies (vec (dom/dependencies signal))})
        bootstrap/not-found))))

(defn get-signal-full
  [req]
  (let [id (Long/parseLong (get-in req [:path-params :id]))]
    (if-let [signal (dom/get-component @state id)]
      (format-response
       req
       {:id id
        :name (dom/component-name signal)
        :value (dom/value @state id)
        :dependencies
        (mapv
         (fn [id]
           (let [s (dom/get-component @state id)
                 v (dom/value @state id)]
             {:id id
              :name (dom/component-name s)
              :value v}))
         (dom/dependencies signal))})
      bootstrap/not-found)))

(defn get-signal-value
  [req]
  (let [id (Long/parseLong (get-in req [:path-params :id]))]
    (if-let [signal (dom/get-component @state id)]
      (format-response req (dom/value @state id))
      bootstrap/not-found)))

(defn add-measurement!
  [req]
  (let [id (Long/parseLong (get-in req [:path-params :id]))]
    (if-let [meter (dom/get-component @state id)]
      (do
        (capture! id (edn/read-string (ring-req/body-string req)))
        (format-response req (dom/value @state id)))
      bootstrap/not-found)))

(defn add-part-measure!
  [req]
  (let [id (Long/parseLong (get-in req [:path-params :id]))
        key (get-in req [:path-params :key])]
    (if-let [meter (dom/get-component @state id)]
      (do
        (capture! id [key (edn/read-string (ring-req/body-string req))])
        (format-response req (dom/value @state id)))
      bootstrap/not-found)))

(defn- parse-json-ext
  "Parse JSON with the transit-style extension for keywords"
  [s]
  (walk/postwalk
   (fn [v]
     (if (and (string? v)
              (re-matches #"~:.*" v))
       (keyword (subs v 2))
       v))
   (json/parse-string s)))

(defn create-signal
  [req]
  (let [spec (parse-json-ext (ring-req/body-string req))
        id (update! #(dom/create-component % spec))]
    (format-response req id)))

(defn update-signal
  [req]
  (let [spec (parse-json-ext (ring-req/body-string req))
        id (Long/parseLong (get-in req [:path-params :id]))]
    (update! #(dom/update-component % id spec))
    (get-signal req)))

(defn delete-signal
  [req]
  (let [id (Long/parseLong (get-in req [:path-params :id]))]
    (try
      (update! #(dom/delete-component % id))
      (format-response req {:success true})
      (catch Exception e
        (format-response req {:success false})))))

(defroutes routes
  [[["/api/signal/:id"
     {:get get-signal
      :put update-signal
      :delete delete-signal}]

    ["/api/signal/:id/full"
     {:get get-signal-full}]

    ["/api/signal/:id/value"
     {:get get-signal-value}]

    ["/api/meter/:id/value"
     {:post add-measurement!}]

    ["/api/meter/:id/value/:key"
     {:post add-part-measure!}]

    ["/api/signal"
     {:post create-signal}]]])

(def service {:env :prod
              ::bootstrap/routes routes
              ::bootstrap/file-path "static"
              ::bootstrap/type :jetty
              ::bootstrap/port 8080})

(defn bootstrap
  "Creates a set of sample data to test with"
  []
  (clear!)
  (add-meter! 'job.a type/TIndicator)
  (add-meter! 'job.b type/TIndicator)
  (add-min-signal! 'all.jobs [0 1])
  (add-max-signal! 'any.job.passed [0 1])
  (add-weighted-signal! 'jobs.weighted [0 1] [0.5 0.5])
  (capture! 0 0)
  (capture! 1 1)
  (let [mid (add-meter! 'jobs.status ::type/multi-indicator)]
    (capture! mid ['job.a 0])
    (capture! mid ['job.b 1])
    (capture! mid ['job.c 1])))

(defn run-dev
  ([] (run-dev 8080))
  ([port]
   (println "\nCreating your [DEV] server...")
   (-> service ;; start with production configuration
       (merge {:env :dev
               ::bootstrap/port port
               ;; do not block thread that starts web server
               ::bootstrap/join? false
               ;; Routes can be a function that resolve routes,
               ;;  we can use this to set the routes to be reloadable
               ::bootstrap/routes #(deref #'routes)
               ;; all origins are allowed in dev mode
               ::bootstrap/allowed-origins {:creds true :allowed-origins (constantly true)}})
       ;; Wire up interceptor chains
       bootstrap/default-interceptors
       bootstrap/dev-interceptors
       bootstrap/create-server
       bootstrap/start)))

(defn stop-dev [service] (bootstrap/stop service))

(defonce runnable-service (bootstrap/create-server service))

(defn -main
  "The entry-point for 'lein run'"
  [& args]
  (let [args (apply hash-map args)]
    (when (or (args "--file") (args "-f"))
      (reset! persistence (persist/file-persistence (or (args "--file")
                                                        (args "-f"))))
      (load!))
    (bootstrap/start runnable-service)))
