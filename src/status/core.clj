(ns status.core
  (:require [clojure.edn :as edn]

            [io.pedestal.http :as bootstrap]
            [io.pedestal.http.route :as route]
            [io.pedestal.http.body-params :as body-params]
            [io.pedestal.http.route.definition :refer [defroutes]]

            [ring.util.request :as ring-req]
            [ring.util.response :as ring-resp]

            [status.domain :as dom]))

;; Try out Clojure 1.9 with schema support

;; API

;; - POST /signal/<id>
;;   adds a new measurement

;; - GET  /signal/<id>/latest
;;   gets the "current" measurement

;; - GET  /signal/<id>/<no>
;;   gets a historic measurement

;; - GET  /indicator/<id>
;;   gets the "current" indicator status

(defn new-system []
  {:signals {}
   :next-id 0})

;; Commands & Queries

(defonce state (ref (dom/new-system)))

(defn clear! []
  (dosync
   (ref-set state (dom/new-system))))

(defn add-meter! [name]
  (dosync
   (let [[c sys] (dom/make-meter @state name)]
     (ref-set state sys)
     c)))

(defn add-min-signal! [name inputs]
  (dosync
   (let [[c sys] (dom/make-min-signal @state name inputs)]
     (ref-set state sys)
     c)))

(defn add-max-signal! [name inputs]
  (dosync
   (let [[c sys] (dom/make-max-signal @state name inputs)]
     (ref-set state sys)
     c)))

(defn capture! [id value]
  (dosync
   (alter state dom/sys-capture id value)))

;; Web handlers

(defn get-signal
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
  [[["/signal/:id/value"
     {:get get-signal}]
    ["/meter/:id/value"
     {:post add-measurement!}]]])

(def service {:env :prod
              ::bootstrap/routes routes
              ::bootstrap/file-path "static"
              ::bootstrap/type :jetty
              ::bootstrap/port 8080})

(defn run-dev []
  (println "\nCreating your [DEV] server...")
  (-> service ;; start with production configuration
      (merge {:env :dev
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
      bootstrap/start))

(defonce runnable-service (bootstrap/create-server service))

(defn -main
  "The entry-point for 'lein run'"
  [& args]
  (println "\nCreating your server...")
  (bootstrap/start runnable-service))
