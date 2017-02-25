(ns status.core
  (:require [clojure.edn :as edn]

            [io.pedestal.http :as bootstrap]
            [io.pedestal.http.route :as route]
            [io.pedestal.http.body-params :as body-params]
            [io.pedestal.http.route.definition :refer [defroutes]]

            [ring.util.request :as ring-req]
            [ring.util.response :as ring-resp]

            [hiccup.core :as html]

            [status.types :as type]
            [status.domain :as dom]
            [status.persistence :as persist]))

;; Commands & Queries

(defonce state (ref (dom/new-system)))
(def persistence (atom nil))

(defn save! []
  (when @persistence
    (persist/save-system @persistence @state)))

(defn load! []
  (when @persistence
    (let [sys (persist/load-system @persistence)]
      (dosync
       (ref-set state sys)))))

(defn clear! []
  (dosync
   (ref-set state (dom/new-system)))
  (future (save!)))

(defn add-meter!
  ([name] (add-meter! name type/TAny))
  ([name type]
   (let [result (dosync
                 (let [[c sys] (dom/make-meter @state name type)]
                   (ref-set state sys)
                   c))]
     (future (save!))
     result)))

(defn add-min-signal! [name inputs]
  (let [result (dosync
                (let [[c sys] (dom/make-min-signal @state name inputs)]
                  (ref-set state sys)
                  c))]
    (future (save!))
    result))

(defn add-max-signal! [name inputs]
  (let [result (dosync
                (let [[c sys] (dom/make-max-signal @state name inputs)]
                  (ref-set state sys)
                  c))]
    (future (save!))
    result))

(defn capture! [id value]
  (dosync
   (alter state dom/sys-capture id value)))

;; Web handlers

(defn all-signals []
  (let [ss @state]
    (bootstrap/json-response
     (vec
      (sort-by :name
               (for [[id comp] (dom/components ss)]
                 {:id id
                  :name (dom/component-name comp)
                  :value (dom/value comp ss)}))))))

(defn get-signal
  [req]
  (if (= "all" (get-in req [:path-params :id]))
    (all-signals)
    (let [id (Long/parseLong (get-in req [:path-params :id]))]
      (if-let [signal (dom/get-component @state id)]
        (bootstrap/json-response {:id id
                                  :name (dom/component-name signal)
                                  :value (dom/value signal @state)
                                  :dependencies
                                  (if (satisfies? dom/Dependent signal)
                                    (vec (dom/dependencies signal))
                                    [])})
        bootstrap/not-found))))

(defn get-signal-full
  [req]
  (let [id (Long/parseLong (get-in req [:path-params :id]))]
    (if-let [signal (dom/get-component @state id)]
      (bootstrap/json-response {:id id
                                :name (dom/component-name signal)
                                :value (dom/value signal @state)
                                :dependencies
                                (if (satisfies? dom/Dependent signal)
                                  (mapv
                                   (fn [id]
                                     (let [s (dom/get-component @state id)
                                           v (dom/value s @state)]
                                       {:id id
                                        :name (dom/component-name s)
                                        :value v}))
                                   (dom/dependencies signal))
                                  [])})
      bootstrap/not-found)))

(defn get-signal-value
  [req]
  (let [id (Long/parseLong (get-in req [:path-params :id]))]
    (if-let [signal (dom/get-component @state id)]
      (bootstrap/edn-response (dom/value signal @state))
      bootstrap/not-found)))

(defn add-measurement!
  [req]
  (let [id (Long/parseLong (get-in req [:path-params :id]))]
    (if-let [meter (dom/get-component @state id)]
      (do
        (capture! id (edn/read-string (ring-req/body-string req)))
        (bootstrap/edn-response (dom/value (dom/get-component @state id) @state)))
      bootstrap/not-found)))

(defroutes routes
  [[["/api/signal/:id"
     {:get get-signal}]

    ["/api/signal/:id/full"
     {:get get-signal-full}]

    ["/api/signal/:id/value"
     {:get get-signal-value}]

    ["/api/meter/:id/value"
     {:post add-measurement!}]

    ]])

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
  (capture! 0 0)
  (capture! 1 1))

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
