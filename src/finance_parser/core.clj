(ns finance-parser.core
  (:gen-class)
  (:require [pdfboxing.text :as text]
            [clojure.string :as s]
            [finance-parser.util :as u]
            [finance-parser.wf_statement :refer [parse-statement-text]]
            [finance-parser.cli :refer [exit validate-args]]
            [clojure.java.io :as io]))

(defn to-cache-file
  [file]
  (str "cache/" (s/replace (.getName file) "pdf" "edn")))

(defn to-cache-path
  [file-path]
  (str "cache/" (s/replace (.getName (io/file file-path)) "pdf" "edn")))

(defn parse-and-cache
  [{:keys [file-path]}]
  (let [cache-path (to-cache-path file-path)
        statement (-> file-path text/extract parse-statement-text)]
    (u/serialize cache-path statement)))

(defn parse-and-print
  [{:keys [file-path]}]
  (let [{:keys [activity transactions]} (-> file-path text/extract parse-statement-text)]
    (u/header-print-list "Activity" activity)
    (u/header-print-list "Transactions" transactions)))

(defn parse-and-cache-dir
  [{:keys [directory]}]
  (let [files (file-seq (io/file directory))]
    (doseq [x files]
      (let [parse-path (.getPath x)
            cache-path (to-cache-file x)
            statement (-> parse-path text/extract parse-statement-text)]
        (u/serialize cache-path statement)))))

(defn read-from-cache
  [file-path]
  (let [cache-path (to-cache-path file-path)]
    (u/deserialize cache-path)))

(defn -main [& args]
  (let [{:keys [action options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (case action
        "cache" (parse-and-cache options)
        "cache-dir" (parse-and-cache-dir options)
        "print" (parse-and-print options)))))
