(ns finance-parser.cli
  (:gen-class)
  (:require [clojure.tools.cli :refer [parse-opts]]
            [finance-parser.util :as u]
            [clojure.string :as s]))

(def command-strings #{"cache" "cache-dir" "print" "server" "transactions" "verify"})
(def sample-pdf-path "sample_statement.pdf")

(def cli-options
  [["-f" "--file-path INPUT_FILE" "Statement PDF to parse"]
   ["-d" "--directory INPUT_DIRECTORY" "Directory with PDF statements to parse"]])

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (s/join \newline errors)))

(defn usage [options-summary]
  (->> ["This is my program. There are many like it, but this one is mine."
        ""
        "Usage: program-name [options] action"
        ""
        "Options:"
        options-summary
        ""
        "Actions:"
        "  cache      save parsed transactions as edn"
        "  cache-dir  (UNDER DEVELOPMENT) save parsed transactions for all statements in directory"
        ""
        "Please refer to the manual page for more information."]
       (s/join \newline)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with a error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [args]
  (let [{:keys [options arguments errors summary] :as result} (parse-opts args cli-options)]
    (cond
      (:help options) ; help => exit OK with usage summary
      {:exit-message (usage summary) :ok? true}
      errors ; errors => exit with description of errors
      {:exit-message (error-msg errors)}
      ;; custom validation on arguments
      (and (= 1 (count arguments))
           (command-strings (first arguments)))
      {:action (first arguments) :options options}
      :else ; failed custom validation => exit with usage summary
      {:exit-message (usage summary)})))
