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

(defn to-cache-path-from-path
  [file-path]
  (to-cache-path-from-file (io/file file-path)))

(defn parse-statement-path
  "returns a statement"
  [file-path]
  (-> file-path text/extract parse-statement-text))

(defn parse-and-cache
  [{:keys [file-path]}]
  (u/serialize (to-cache-path-from-path file-path) (parse-statement-path file-path)))

(defn print-statement
  [{:keys [activity transactions]}]
  (u/header-print-list "Activity" activity)
  (u/header-print-list "Transactions" transactions))

(defn parse-and-cache-dir
  [{:keys [directory]}]
  (doseq [statement-file (file-seq (io/file directory))]
    (u/serialize (to-cache-path-from-file statement-file)
                 (parse-statement-path (.getPath statement-file)))))

(defn options-to-statement-files
  [options]
  (println options)
  (cond
    (contains? options :directory) (file-seq (io/file (:directory options)))
    (contains? options :file-path) [(io/file (:file-path options))]
    :else []))

(defn parse-statement-file
  [statement-file]
  (parse-statement-path (.getPath statement-file)))

(defn statements-to-transactions
  [statements]
  (reduce
    concat
    []
    (map :transactions statements)))

(defn transactions-cmd
  [options]
  (->> options
      options-to-statement-files
      (map parse-statement-file)
      (u/header-print-list "Statements")
      statements-to-transactions
      (u/header-print-list "blah")
      ))

(defn verify-cmd
  [options]
  (->> options
      options-to-statement-files
      (u/header-print-list "Statement files")))

(defn read-from-cache
  [file-path]
  (let [cache-path (to-cache-path-from-path file-path)]
    (u/deserialize cache-path)))

(defn app [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    "hello HTTP!"})

(defn -main [& args]
  (let [{:keys [action options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (case action
        "transactions" (transactions-cmd options)
        "verify" (verify-cmd options)

        "cache" (parse-and-cache options)
        "cache-dir" (parse-and-cache-dir options)
        "print" (-> options :file-path parse-statement-path print-statement)
        "server" (run-server app {:port 8080})
        ))))
