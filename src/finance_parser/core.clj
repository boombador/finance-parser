(ns finance-parser.core
  (:gen-class)
  (:use [org.httpkit.server :only [run-server]])
  (:require [pdfboxing.text :as text]
            [clojure.string :as s]
            [finance-parser.util :as u]
            [finance-parser.wf_statement :refer [parse-statement-text]]
            [finance-parser.cli :refer [exit validate-args]]
            [clojure.java.io :as io]
            ))

(defn to-cache-path-from-file
  [file]
  (str "cache/" (s/replace (.getName file) "pdf" "edn")))

(defn parse-statement-path
  "returns a statement"
  [file-path]
  (-> file-path text/extract parse-statement-text))

(defn print-statement
  [{:keys [activity transactions]}]
  (u/header-print-list "Activity" activity)
  (u/header-print-list "Transactions" transactions))

(defn options-to-statement-files
  [options]
  (println options)
  (cond
    (contains? options :directory) (file-seq (io/file (:directory options)))
    (contains? options :file-path) [(io/file (:file-path options))]
    :else []))

(defn parse-statement-file
  [statement-file]
  (-> (parse-statement-path (.getPath statement-file))
      (assoc :source-filename (.getName statement-file))))

(defn statements-to-transactions
  [statements]
  (let [transaction-seqs (map :transactions statements)]
    (reduce concat [] transaction-seqs)))

(defn valid-statement-file
  [file]
  (not (.isDirectory file)))

(defn load-statement-file
  [file]
  (let [cache-file (io/file (to-cache-path-from-file file))
        cache-path (.getPath cache-file)]
    (if (.exists cache-file)
      (u/deserialize cache-path)  ;; TODO: check staleness by sha contents
      (let [parsed-statement (parse-statement-file file)]
        (do
          (u/serialize cache-path parsed-statement)
          parsed-statement)))))

(defn files-to-statements
  [files]
  (->> files
       (filter valid-statement-file)
       (map load-statement-file)
       (sort-by :source-filename)))

(defn transactions-cmd
  [options]
  (->> options
       options-to-statement-files
       files-to-statements
       statements-to-transactions
       (u/header-print-list "Transactions")))

(defn verify-cmd
  [options]
  (->> options
      options-to-statement-files
      (u/header-print-list "Statement files")))

(defn print-cmd
  [options]
  (-> options
      options-to-statement-files
      files-to-statements
      first
      print-statement))

;(defn app [req]
  ;{:status  200
   ;:headers {"Content-Type" "text/html"}
   ;:body    "hello HTTP!"})
;"server" (run-server app {:port 8080})

(defn -main [& args]
  (let [{:keys [action options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (case action
        "transactions" (transactions-cmd options)
        "verify" (verify-cmd options)
        "print" (print-cmd options)))))
