(defproject finance-parser "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [pdfboxing "0.1.14.1-SNAPSHOT"]
                 [org.clojure/tools.cli "0.4.1"]
                 [http-kit "2.3.0"]
                 [compojure "1.6.0"]
                 [garden "1.3.3"]
                 [org.clojure/clojurescript "1.10.238"]
                 [ring/ring-core "1.6.2"]]
  :main finance-parser.core
  ;:main ^:skip-aot finance-parser.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :cljsbuild {:builds [{:compiler {:asset-path "js/out"
                                   :main "finance-parser.core"
                                   :optimizations :none
                                   :output-to "resources/public/js/main.js"
                                   :output-dir "resources/public/js/out"}
                        :figwheel true  ;{:websocket-host "web-clj.local"}
                        :id "dev"
                        :source-paths ["src/cljs"]}]}
  :figwheel {:css-dirs ["resources/public/css"]
             :hawk-options {:watcher :polling}
             :ring-handler finance-parser.core/app
             :server-port 3000}
  :garden {:builds [{:id "screen"
                     :source-paths ["src/garden"]
                     :stylesheet web-clj.core/main
                     :compiler {:output-to "resources/public/css/main.css"
                                :pretty-print? false}}]}
  :min-lein-version "2.8.1"
  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-figwheel "0.5.16"]
            [lein-garden "0.3.0"]
            [lein-ring "0.12.1"]]
  :resource-paths ["resources"]
  :source-paths ["src/clj"])
