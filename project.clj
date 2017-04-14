(defproject status "0.1.0-SNAPSHOT"
  :plugins [[lein-cloverage "1.0.9"]
            [lein-cljsbuild "1.1.5"]
            [lein-figwheel "0.5.9"]]
  :cljsbuild {:builds [{:id "dev"
                        :source-paths ["src"]
                        :figwheel true
                        :compiler {:main web.core
                                   :output-to "static/cljs/status.js"
                                   :output-dir "static/cljs"
                                   :asset-path "cljs"
                                   :optimizations :none
                                   :source-map true}}]}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [cheshire "5.5.0"]
                 [clojurewerkz/quartzite "2.0.0"]

                 [ring/ring-core "1.4.0"]
                 [clj-http "2.3.0"]

                 [hiccup "1.0.5"]

                 [com.taoensso/timbre "4.8.0"]

                 [io.pedestal/pedestal.jetty "0.4.1"]
                 [io.pedestal/pedestal.service "0.4.1"]
                 [io.pedestal/pedestal.service-tools "0.4.1"]

                 ;; Web dependencies
                 [org.clojure/clojurescript "1.9.494"]
                 [org.omcljs/om "0.9.0"]
                 [secretary "1.2.3"]])
