(ns status.core
  (:require [clojure.edn :as edn]

            [io.pedestal.http :as bootstrap]
            [io.pedestal.http.route :as route]
            [io.pedestal.http.body-params :as body-params]
            [io.pedestal.http.route.definition :refer [defroutes]]

            [ring.util.request :as ring-req]
            [ring.util.response :as ring-resp]

            [hiccup.core :as html]

            [status.domain :as dom]))

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

(defn view-signal
  [req]
  (let [id (Long/parseLong (get-in req [:path-params :id]))]
    (if-let [signal (dom/get-component @state id)]
      {:status 200
       :headers {"Content-Type" "text/html"}
       :body (html/html
              [:html
               [:head
                [:link {:rel "stylesheet"
                        :href "/bootstrap-3.3.7/css/bootstrap.min.css"}]
                [:link {:rel "stylesheet"
                        :href "/site/site.css"}]]
               [:body
                [:div.container
                 [:div.jumbotron
                  [:h2 (str (dom/component-name signal))]]

                 [:div.row
                  [:div.col-lg-6.col-lg-offset-3
                   (let [v (dom/value signal @state)]
                     (cond
                       (nil? v) [:div.alert.status.alert-warning "Undefined"]
                       (= 1 v) [:div.alert.status.alert-success "100%"]
                       (> v 0.5) [:div.alert.status.alert-warning (format "%.0f%%" (* 100.0 v))]
                       :else [:div.alert.status.alert-danger (format "%.0f%%" (* 100.0 v))]))]]

                 (when (satisfies? dom/Dependent signal)
                   [:div.row
                    [:div.col-lg-6.col-lg-offset-3
                     [:h4 "Dependencies"]
                     [:table.table
                      [:thead
                       [:tr [:th "Name"] [:th "Status"]]]
                      [:tbody
                       (for [id (dom/dependencies signal)]
                         (let [sig (dom/get-component @state id)
                               v (dom/value sig @state)]
                           [:tr {:class (cond
                                          (= 1 v) "success"
                                          (or (nil? v) (> v 0.5)) "warning"
                                          :else "danger")}
                            [:td (dom/component-name sig)]
                            [:td (if (nil? v)
                                   "Undefined"
                                   (format "%.0f%%" (* 100.0 v)))]]))]]]])]
                [:script {:src "/jquery-1.12.4/jquery.min.js"}]
                [:script {:src "/bootstrap-3.3.7/js/bootstrap.min.js"}]]])}
      bootstrap/not-found)))

(defroutes routes
  [[["/api/signal/:id/value"
     {:get get-signal}]

    ["/api/meter/:id/value"
     {:post add-measurement!}]

    ["/web/signal/:id"
     {:get view-signal}]

    ]])

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
