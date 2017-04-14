(ns ^:figwheel-always web.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [goog.dom :as gdom]
            [goog.net.XhrIo :as xhr]
            [goog.events :as events]
            [goog.History :as history]
            [goog.history.EventType :as EventType]
            [cljs.core.async :refer [put! chan <!]]
            [cljs.reader :as edn]
            [secretary.core :as secretary :refer-macros [defroute]]
            [clojure.string :as string]))

(enable-console-print!)

(println "<<< Loading status.web.core namespace >>>")

(secretary/set-config! :prefix "#")

(defonce status-state
  (atom nil))

(println "<<< The end >>>")

(defn load-status [id]
  (xhr/send (str "/api/signal/" id "/full?format=edn")
            (fn [event]
              (reset! status-state
                      (-> event .-target .getResponseText edn/read-string)))))

(defn load-all []
  (xhr/send (str "/api/signal/all?format=edn")
            (fn [event]
              (reset! status-state
                      (-> event .-target .getResponseText edn/read-string)))))

(defn formatted-value [val]
  (when val (int (* 100 val))))

(defn warning? [val] (or (nil? val)
                         (< 0.5 val 1)))
(defn error? [val] (<= val 0.5))
(defn success? [val] (= 1 val))

(defn- div [clazz & elements]
  (apply dom/div #js {:className clazz} elements))

(defn- container [& elements]
  (apply div "container" elements))

(defn- row [& elements]
  (apply div "row" elements))

(defn nav-view [_ owner]
  (reify
    om/IRender
    (render [_]
      (dom/div
       #js {:className "navbar navbar-inverse navbar-fixed-top"}
       (container
        (dom/div
         #js {:className "navbar-header"}
         (dom/a #js {:className "navbar-brand" :href "#/"} "Statizer")))))))

(om/root
 nav-view
 status-state
 {:target (.getElementById js/document "nav")})

(declare signal-path)

(defn- status-table [signals]
  (dom/table
   #js {:className "table"}
   (dom/thead nil
              (dom/tr nil
                      (dom/td nil "Name")
                      (dom/td nil "Status")))
   (dom/tbody nil
              (for [x signals]
                (dom/tr nil
                        (dom/td nil
                                (if (:id x)
                                  (dom/a
                                   #js {:href (signal-path {:id (:id x)})}
                                   (str (:name x)))
                                  (str (:name x))))
                        (dom/td
                         #js {:className (cond
                                           (map? (:value x)) "status"
                                           (success? (:value x)) "success status"
                                           (warning? (:value x)) "warning status"
                                           :else "danger status")}
                         (if (map? (:value x))
                           (str "Multi-valued: " (count (:value x))
                                (if (= 1 (count (:value x)))
                                  " entry"
                                  " entries"))
                           (formatted-value (:value x)))))))))

(defn single-view [data owner]
  (reify
    om/IRender
    (render [_]
      (container
       (row
        (div "col-lg-6 col-lg-offset-3 status-heading"
         (dom/h2 nil (str (:name data)))))

       (if (map? (:value data))
         (row
          (div
           "col-lg-6 col-lg-offset-3"
           (dom/h4 nil "Status")
           (status-table (map (fn [[k v]] {:name k :value v}) (:value data)))))

         (row
          (div
           "col-lg-6 col-lg-offset-3"
           (dom/h4 nil "Status")
           (dom/div
            #js {:className (cond
                              (success? (:value data)) "alert status alert-success"
                              (warning? (:value data)) "alert status alert-warning"
                              :else "alert status alert-danger")}
            (formatted-value (:value data))))))

       (when (seq (:dependencies data))
         (row
          (div "col-lg-6 col-lg-offset-3"
               (dom/h4 nil "Dependencies")
               (status-table (:dependencies data)))))))))

(defroute signal-path "/signal/:id" [id]
  (load-status id)
  (om/root
   single-view
   status-state
   {:target (. js/document (getElementById "app"))}))

(defn status-view [data owner]
  (reify
    om/IRender
    (render [_]
      (container
       (row
        (dom/div
         #js {:className "page-header status-heading"}
         (dom/h1 nil "Statizer")))

       (status-table data)

       (dom/button
        #js {:className "btn btn-primary"
             :onClick #(load-all)}
        "Refresh")))))

(defroute root-path "/" {}
  (load-all)
  (om/root
   status-view
   status-state
   {:target (. js/document (getElementById "app"))}))

(let [h (goog.History.)]
  (goog.events/listen h EventType/NAVIGATE #(secretary/dispatch! (.-token %)))
  (doto h (.setEnabled true)))


