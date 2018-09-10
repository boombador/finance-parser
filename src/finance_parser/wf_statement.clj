(ns finance-parser.wf_statement
  (:gen-class)
  (:require [pdfboxing.text :as text]
            [clojure.string :as s]
            [finance-parser.util :refer [deep-merge] :as u]
            ))

(def section-defs
  [{:section-name :intro :next-start "activity summary"}
   {:section-name :activity :next-start "transaction history"}
   {:section-name :transactions :next-start "ending balance"}
   {:section-name :ending-balance :next-start "summary of checks written"}
   {:section-name :checks :next-start "worksheet to balance your account"}
   {:section-name :worksheet :next-start nil}])

(def month-date-regex #"^\d+\/\d+")
(def header-line-regex #"page \d+ of \d+")
;modified version of #"^[+-]?[0-9]{1,3}(?:,?[0-9]{3})*\.[0-9]{2}$"
;https://stackoverflow.com/questions/354044/what-is-the-best-u-s-currency-regex
(def money-regex #"[0-9]{1,3}(?:,?[0-9]{3})*\.[0-9]{2}")

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

(defn numeric? [s]
  (if-let [s (seq s)]
    (let [s (if (= (first s) \-) (next s) s)
          s (drop-while #(Character/isDigit %) s)
          s (if (= (first s) \.) (next s) s)
          s (drop-while #(Character/isDigit %) s)]
      (empty? s))))

(defn parse-int [s]
  (Integer/parseInt (re-find #"\A-?\d+" s)))

(defn parse-float [s]
  (-> s
      (s/replace #"," "")
      Float/parseFloat))

(defn parse-transaction-line
  [line]
  (let [split-line (s/split line #"\s+")
        month-string (first split-line)
        remainder-without-date (rest split-line)
        is-check (and
                   (= "check" (nth remainder-without-date 1))
                   (> (count remainder-without-date) 1)
                   (numeric? (nth remainder-without-date 0)))
        transaction-type (if is-check :check :unknown)
        check-number (if is-check (parse-int (nth remainder-without-date 0)) nil)
        remainder-without-check (if is-check (drop 2 remainder-without-date) remainder-without-date)
        money-values (reverse (take-while #(re-matches money-regex %) (reverse remainder-without-check)))
        remainder-without-money (drop-last (count money-values) remainder-without-check)]
    {:date month-string
     :type transaction-type
     :check-number check-number
     :money money-values
     :description (s/join " " remainder-without-money)}))

(defn number-to-coefficients
  [number]
  (let [base-two-string (Integer/toString number 2)]
    (map #(if (= % "1") 1 -1) base-two-string)))

(defn expt [x n]
  (reduce * (repeat n x)))

(defn make-check-solution
  [deltas next-balance]
  (fn [coefficients]
    (let [sign-adjusted-deltas (map * coefficients deltas)
          calculated-balance (reduce + 0 sign-adjusted-deltas)]
      (= next-balance calculated-balance))))

(defn get-valid-coefficients
  [prior-balance next-balance deltas]
  (let [possibilities (expt 2 (count deltas))
        check-solution (make-check-solution deltas next-balance)
        possible-coefficient-lists (map number-to-coefficients (range possibilities))]
    (filter check-solution possible-coefficient-lists)))

(defn classify-withdrawal-or-deposit
  [[prior-balance processed-transactions] [date-string day-transactions]]
  (let [new-balance (-> day-transactions last :money last)
        coefficients-solutions (get-valid-coefficients prior-balance new-balance (map #(-> % :money first parse-float) day-transactions))
        ;updated (if (= 1 (count coefficients-solutions)) )
        ;latest-transactions (map #() day-transactions)
        ]
    [new-balance []]))

(defn date-key
  [[date-string _]]
  (let [date-parts (s/split date-string #"\/")
        [months days] (map parse-int date-parts)]
    (+ days (* 35 months))))

(defn fill-transactions
  "hello there"
  [activity raw-transactions]
  (let [start-balance (get-in activity [:account-balance :start :amount])
        transactions-by-date (group-by :date raw-transactions)
        sorted-date-transactions (sort-by date-key (seq transactions-by-date))
        [_ updated-transactions] (reduce classify-withdrawal-or-deposit [start-balance []] sorted-date-transactions)]
    updated-transactions))

(defn parse-transactions
  "returns list containing the summary map and the remaining document text"
  [activity main-text]
  (let [transaction-split-lines (trim-transaction-text main-text)
        [_ transaction-joined-lines] (reduce
                                       fold-transaction-text-line
                                       [nil []]
                                       (reverse transaction-split-lines))
        cleaned-transaction-lines (map clean-transaction transaction-joined-lines)
        raw-transactions (map parse-transaction-line cleaned-transaction-lines)
        transactions (fill-transactions activity raw-transactions)] ;; TODO: finish
    raw-transactions))

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
        transactions (parse-transactions activity transactions-text)]
    {:activity activity :transactions transactions}))
