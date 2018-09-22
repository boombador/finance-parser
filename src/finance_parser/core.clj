(ns finance-parser.core
  (:gen-class)
  (:require [pdfboxing.text :as text]
            [clojure.string :as s]
            [finance-parser.util :refer [header-print header-print-list]]
            [finance-parser.wf_statement :refer [parse-statement]]))

(def sample-pdf-path "sample_statement.pdf")
(def sample-text-path "sample_statement_raw.txt")

(defn display-transaction
  [{:keys [date money]}]
  {:date date :money money}
  (format "%s\t%s" date (s/join " " (map #(format "%s\t" %) money))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [extracted-text (-> sample-pdf-path text/extract)
        {:keys [activity transactions]} (parse-statement extracted-text)]
    (header-print-list "activity" activity)
    (header-print-list "transactions" transactions)))
