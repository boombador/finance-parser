(ns finance-parser.core
  (:gen-class)
  (:require [pdfboxing.text :as text]
            [clojure.string :as s]
            [finance-parser.util :refer [header-print header-print-list]]
            [finance-parser.wf_statement :refer [parse-statement]]))

(def sample-pdf-path "sample_statement.pdf")

(defn display-transaction
  [{:keys [date money]}]
  {:date date :money money}
  (format "%s\t%s" date (s/join " " (map #(format "%s\t" %) money))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [{:keys [activity transactions]} (-> sample-pdf-path text/extract parse-statement)]
    ;(header-print "Activity" activity)
    ;(header-print-list "transactions" (map display-transaction transactions))
    (println "done procesing")))
