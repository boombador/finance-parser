(defproject finance-parser "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [pdfboxing "0.1.14.1-SNAPSHOT"]
                 [org.clojure/tools.cli "0.4.1"]
                 [http-kit "2.3.0"]
                 ]
  :main ^:skip-aot finance-parser.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
