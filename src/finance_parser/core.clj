(ns finance-parser.core
  (:gen-class)
  (:require [pdfboxing.text :as text]
            [clojure.string :as s]))

(def banner "========================================================================\n")

(defn deep-merge [v & vs]
  (letfn [(rec-merge [v1 v2]
            (if (and (map? v1) (map? v2))
              (merge-with deep-merge v1 v2)
              v2))]
    (when (some identity vs)
      (reduce #(rec-merge %1 %2) v vs))))

(defn parse-intro
  "returns list containing the summary map and the remaining document text"
  [processed-text]
  (let [index-of-remainder (s/index-of processed-text "activity summary")
        main-text (subs processed-text 0 (dec index-of-remainder))
        remainder (subs processed-text index-of-remainder)]
    ;(println main-text)
    [{} remainder]))

;;; Summary line parsing functions

(def space #"\s+")

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

(defn parse-summary
  "returns list containing the summary map and the remaining document text"
  [processed-text]
  (let [index-of-remainder (s/index-of processed-text "transaction history")
        main-text (subs processed-text 0 (dec index-of-remainder))
        remainder (subs processed-text index-of-remainder)
        lines (rest (s/split-lines main-text))
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
        line-results (map (fn [line parser] (parser line)) lines parsers)
        data (reduce (fn [merged item]
                       (if item
                         (deep-merge merged item)
                         merged))
                     {}
                     line-results)]
    [data remainder]))

(def month-date-regex #"^\d+\/\d+\w+")

(defn transaction-start-line?
  [line]
  (re-find month-date-regex line))

(defn fold-transaction-text-line
  [[builder built-list] line]
  (let [new-builder (if builder (str line builder) line)
        new-list (cons new-builder built-list)]
    (if (transaction-start-line? line)
      [nil new-list]
      [new-builder built-list])))

(defn clean-transaction
  [transaction-string]
  (s/replace transaction-string #"\n" " "))

(defn parse-history
  "returns list containing the summary map and the remaining document text"
  [processed-text]
  (let [index-of-remainder (s/index-of processed-text "summary of checks written")
        main-text (subs processed-text 0 (dec index-of-remainder))
        remainder (subs processed-text index-of-remainder)
        transaction-split-lines (->> main-text
                                     s/split-lines
                                     (drop 3)
                                     (take-while #(not (s/starts-with? % "ending balance"))))
        transactions (->> (reverse transaction-split-lines)
                          (reduce
                            fold-transaction-text-line
                            [nil []])
                          second
                          (map clean-transaction))]
    [transactions remainder]))

(defn parse-check-summary
  "returns list containing the summary map and the remaining document text"
  [processed-text]
  (let [index-of-remainder (s/index-of processed-text "worksheet to balance your account")
        main-text (subs processed-text 0 (dec index-of-remainder))
        remainder (subs processed-text index-of-remainder)
        lines (rest (s/split-lines main-text))]
    [{} remainder]))

(def section-defs
  [{:section-name "Intro" :next-start "activity summary"}
   {:section-name "Activity Summary" :next-start "transaction history"}
   {:section-name "Transaction History" :next-start "summary of checks written"}
   {:section-name "Check Summary" :next-start "worksheet to balance your account"}
   {:section-name "Balance Worksheet" :next-start nil}])

(defn clean-raw-text
  [raw-text]
  (as-> raw-text v
    (s/lower-case v)
    ;(s/split-lines v)
    ;(filter #(not (re-matches #"^\w*$" %)) v) ; empty lines
    ;(filter #() v) ; page number lines
    ))

(defn split-text
  "(In progress, not used yet) split text into labelled sections to simplify parse functions"
  [clean-text]
  (reduce (fn [[remaining-text processed] {:keys [section-name next-start]}]
            (let [text-length (count remaining-text)
                  index-of-remainder (if next-start
                                       (s/index-of remaining-text next-start)
                                       (inc text-length))
                  no-more-remainder (> index-of-remainder text-length)
                  main-text (subs remaining-text 0 (dec index-of-remainder))
                  new-remainder (if no-more-remainder
                                  nil
                                  (subs remaining-text index-of-remainder))
                  new-processed (assoc processed section-name main-text)]
              [new-remainder new-processed]))
          [clean-text {}]
          section-defs))

(def sample-pdf-path "sample_statement.pdf")

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [processed-text (-> sample-pdf-path text/extract clean-raw-text)
        [intro summary-text] (parse-intro processed-text) 
        [summary history-text] (parse-summary summary-text) 
        [history check-text] (parse-history history-text) 
        ; [check-summary worksheet-text] (parse-check-summary check-text) 
        ]
    (println summary)
    (println (str "Transaction count: " (count history)))
    (doseq [x history]
      (println x))
    ;(println (split-text processed-text))
    ))





