(ns finance-parser.wf_statement
  (:gen-class)
  (:require [pdfboxing.text :as text]
            [clojure.string :as s]
            [finance-parser.util :refer [header-print deep-merge]]))

(def section-defs
  [{:section-name :intro :next-start "activity summary"}
   {:section-name :activity :next-start "transaction history"}
   {:section-name :transactions :next-start "ending balance"}
   {:section-name :ending-balance :next-start "summary of checks written"}
   {:section-name :checks :next-start "worksheet to balance your account"}
   {:section-name :worksheet :next-start nil}])

(def month-date-regex #"^\d+\/\d+")

(def header-line-regex #"page \d+ of \d+")

(defn transaction-start-line?
  [line]
  (re-find month-date-regex line))

(defn page-header-line?
  [line]
  (re-find header-line-regex line))

(defn clean-raw-text
  [raw-text]
  (as-> raw-text v
    (s/lower-case v)
    (s/split-lines v)
    (filter #(not (s/blank? %)) v)
    (filter #(not (page-header-line? %)) v)
    (s/join "\n" v)))

(defn line-to-beginning-balance [line] 
  "example: beginning balance on DATE MONEY"
  (let [[_ _ _ date amount] (s/split line #"\s+")]
    {:account-balance {:start {:date date :amount amount}}}))

(defn line-to-deposits-additions [line] 
  "example: deposits/additions MONEY"
  (let [[_ amount] (s/split line #"\s+")]
    {:deposits amount}))

(defn line-to-withdrawals-subtractions [line] 
  "example: withdrawals/subtractions -  MONEY"
  (let [[_ _ amount] (s/split line #"\s+")]
    {:withdrawals amount}))

(defn line-to-ending-balance [line] 
  "example: ending balance on DATE  MONEY "
  (let [[_ _ _ date amount] (s/split line #"\s+")]
    {:account-balance {:end {:date date :amount amount}}}))

(defn line-to-account-number [line] 
  "example: account number:  NUMBER "
  (let [[_ _ number] (s/split line #"\s+")]
    {:account-number number}))

(defn line-to-name [line] 
  "example: USERNAME"
  {:name line})

(defn line-to-terms [line] 
  "example: JURISDICTION terms and conditions apply "
  {:terms-and-conditions line})

(defn discard-line [line] 
  "example: deposits/additions MONEY"
  nil)

(defn line-to-routing-number [line] 
  "example: routing number (rtn):  NUMBER "
  (let [[_ _ _ number] (s/split line #"\s+")]
    {:routing-number number}))

(defn parse-activity
  "returns list containing the summary map and the remaining document text"
  [main-text]
  (let [lines (rest (s/split-lines main-text))
        parsers [line-to-beginning-balance
                 line-to-deposits-additions
                 line-to-withdrawals-subtractions
                 line-to-ending-balance 
                 line-to-account-number
                 line-to-name
                 line-to-terms
                 discard-line
                 line-to-routing-number
                 discard-line
                 discard-line
                 discard-line]
        line-results (map (fn [line parser] (parser line)) lines parsers)]
    (reduce (fn [merged item]
              (if item
                (deep-merge merged item)
                merged))
            {}
            line-results)))


(defn clean-transaction
  [transaction-string]
  (s/replace transaction-string #"\n" " "))

(defn trim-transaction-text
  [main-text]
  (->> main-text
       s/split-lines
       (drop 3)))

(defn fold-transaction-text-line
  [[builder built-list] line]
  (let [new-builder (if builder (str line " " builder) line)
        new-list (cons new-builder built-list)]
    (if (transaction-start-line? line)
      [nil new-list]
      [new-builder built-list])))

(defn parse-transaction-line
  [line]
  (let [split-line (s/split line #"\s+")
        month-string (first split-line)
        remainder (rest split-line)]
    {:original line :date month-string :rest remainder}))

(defn parse-transactions
  "returns list containing the summary map and the remaining document text"
  [main-text]
  (let [transaction-split-lines (trim-transaction-text main-text)
        [_ transaction-joined-lines] (reduce
                                       fold-transaction-text-line
                                       [nil []]
                                       (reverse transaction-split-lines))
        cleaned-transaction-lines (map clean-transaction transaction-joined-lines)
        transactions (map parse-transaction-line cleaned-transaction-lines)
        result (map :original transactions)]  ; try to keep finicky changes on this line
    result))

(defn fold-section-into-structured
  [[remaining-text processed] {:keys [section-name next-start]}]
  (let [text-length (count remaining-text)
        index-of-remainder (if next-start
                             (s/index-of remaining-text next-start)
                             (inc text-length))
        no-more-remainder (> index-of-remainder text-length)
        main-text (subs remaining-text 0 (dec index-of-remainder))
        new-remainder (if no-more-remainder nil (subs remaining-text index-of-remainder))
        new-processed (assoc processed section-name main-text)]
    [new-remainder new-processed]))

(defn segment-text
  "split text into labelled sections to simplify parse functions"
  [clean-text]
  (second
    (reduce
      fold-section-into-structured
      [clean-text {}]
      section-defs)))

(defn parse-statement
  [pdf-extracted-text]
  (let [segmented-text (-> pdf-extracted-text clean-raw-text segment-text)
        {activity-text :activity transactions-text :transactions} segmented-text
        activity (parse-activity activity-text)
        transactions (parse-transactions transactions-text)]
    {:activity activity :transactions transactions}))
