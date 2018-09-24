(ns finance-parser.core
  (:gen-class)
  (:require [pdfboxing.text :as text]
            [clojure.string :as s]
            [finance-parser.util :as u]
            [finance-parser.wf_statement :refer [parse-statement-text]]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :as io]
            ))


(defn display-transaction
  [{:keys [date money]}]
  {:date date :money money}
  (format "%s\t%s" date (s/join " " (map #(format "%s\t" %) money))))


;(def example-cli-options
  ;[;; First three strings describe a short-option, long-option with optional
   ;;; example argument description, and a description. All three are optional
   ;;; and positional.
   ;["-p" "--port PORT" "Port number"
    ;:default 80
    ;:parse-fn #(Integer/parseInt %)
    ;:validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   ;["-H" "--hostname HOST" "Remote host"
    ;:default (InetAddress/getByName "localhost")
    ;;; Specify a string to output in the default column in the options summary
    ;;; if the default value's string representation is very ugly
    ;:default-desc "localhost"
    ;:parse-fn #(InetAddress/getByName %)]
   ;;; If no required argument description is given, the option is assumed to
   ;;; be a boolean option defaulting to nil
   ;[nil "--detach" "Detach from controlling process"]
   ;["-v" nil "Verbosity level; may be specified multiple times to increase value"
    ;;; If no long-option is specified, an option :id must be given
    ;:id :verbosity
    ;:default 0
    ;;; Use :update-fn to create non-idempotent options (:default is applied first)
    ;:update-fn inc]
   ;;; A boolean option that can explicitly be set to false
   ;["-d" "--[no-]daemon" "Daemonize the process" :default true]
   ;["-h" "--help"]])

;; The :default values are applied first to options. Sometimes you might want
;; to apply default values after parsing is complete, or specifically to
;; compute a default value based on other option values in the map. For those
;; situations, you can use :default-fn to specify a function that is called
;; for any options that do not have a value after parsing is complete, and
;; which is passed the complete, parsed option map as it's single argument.
;; :default-fn (constantly 42) is effectively the same as :default 42 unless
;; you have a non-idempotent option (with :update-fn or :assoc-fn) -- in which
;; case any :default value is used as the initial option value rather than nil,
;; and :default-fn will be called to compute the final option value if none was
;; given on the command-line (thus, :default-fn can override :default)



(defn usage [options-summary]
  (->> ["This is my program. There are many like it, but this one is mine."
        ""
        "Usage: program-name [options] action"
        ""
        "Options:"
        options-summary
        ""
        "Actions:"
        "  cache    save parsed transactions as edn"
        ""
        "Please refer to the manual page for more information."]
       (s/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (s/join \newline errors)))

(defn to-cache-path
  [file-path]
  (str "cache/" (s/replace (.getName (io/file file-path)) "pdf" "edn")))

(defn parse-and-cache
  [{:keys [file-path]}]
  (let [cache-path (to-cache-path file-path)
        statement (-> file-path text/extract parse-statement-text)]
    (u/serialize cache-path statement)))

(defn read-from-cache
  [file-path]
  (let [cache-path (to-cache-path file-path)]
    (u/deserialize cache-path)))

(def sample-pdf-path "sample_statement.pdf")
(def cli-options
  [["-f" "--file-path INPUT_FILE" "Statement PDF to parse"
    :default sample-pdf-path]])

(def command-strings #{"cache" "print"})

(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with a error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [args]
  (let [{:keys [options arguments errors summary] :as result} (parse-opts args cli-options)]
    (u/header-print-list "result" result)
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

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (let [{:keys [action options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (case action
        "cache" (parse-and-cache options)))))
