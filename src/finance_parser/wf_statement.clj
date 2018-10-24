(ns finance-parser.wf_statement
  (:gen-class)
  (:require [pdfboxing.text :as text]
            [clojure.string :as s]
            [finance-parser.util :refer [deep-merge] :as u]
            [clojure.pprint :refer [cl-format]]))

(defn abs [n] (max n (- n)))
(defn to-binary-string
  [str-length x]
  (cl-format nil (str "~" str-length ",'0',B")  x))

(defn expt [x n]
  (reduce * (repeat n x)))

(def section-defs
  [{:section-name :intro :next-start "activity summary" :optional false}
   {:section-name :activity :next-start "transaction history" :optional false}
   {:section-name :transactions :next-start "ending balance" :optional false}
   {:section-name :ending-balance :next-start "summary of checks written" :optional true}
   {:section-name :checks :next-start "worksheet to balance your account" :optional false}
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

(defn strip [coll chars]
  (apply str (remove #((set chars) %) coll)))

(defn parse-money [money-str]
  (parse-float (strip money-str "$,")))

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
  [str-length number]
  (let [second-attempt (to-binary-string str-length number)]
    (map #(if (= % \1) 1 -1) second-attempt)))

(defn date-key
  [[date-string _]]
  (let [date-parts (s/split date-string #"\/")
        [months days] (map parse-int date-parts)]
    (+ days (* 35 months))))

(defn fold-date-transactions-group
  [accumulator item]
  (let [[day-starting-balance modified-groups] accumulator
        [date-string list-of-transactions] item
        day-ending-balance (-> list-of-transactions last :money last parse-money)
        transformed-date-group [day-starting-balance list-of-transactions]
        latest-modified-groups (cons transformed-date-group modified-groups)]
    [day-ending-balance latest-modified-groups]))

(defn transactions-grouped-by-date-to-daily-balance
  [start-balance grouped-transactions]
  (->> grouped-transactions
       (reduce fold-date-transactions-group [start-balance '()] )
       second
       reverse))

(defn group-transactions
  "Return a list of lists where each inner list contains the date string and the transactions
  for that date, where the top-level list is sorted by the date"
  [start-balance transactions]
  (let [sorted-groups (->> transactions
                           (group-by :date)
                           seq
                           (sort-by date-key))]
    (transactions-grouped-by-date-to-daily-balance start-balance sorted-groups)))

(defn make-check-solution
  [prior-balance next-balance deltas]
  (fn [coefficients]
    (->> coefficients
         (map * deltas)
         (reduce + prior-balance)
         ((fn [calculated-balance]
             (abs (- next-balance calculated-balance))))
         ((fn [delta]
            (< delta 0.01))))))

(defn get-valid-coefficients
  "returns the sequence equal in length to deltas containing the coefficients
  belonging in [1,-1] that if the result were called `solution` then:
  
    (= (+ initial (map * solution deltas)) final)"
  [prior-balance next-balance deltas]
  (let [valid-solutions (->> (count deltas)
                                (expt 2)
                                range
                                (map #(number-to-coefficients (count deltas) %))
                                (filter (make-check-solution prior-balance next-balance deltas)))]
    (if (= 1 (count valid-solutions))
      (first valid-solutions)
      nil)))

(defn set-transaction-sign
  [transaction coefficient]
  (assoc transaction
         :withdrawal-or-deposit
         (if (= coefficient 1) :deposit :withdrawal)))

(defn to-ending-balance
  [date-transactions]
  (-> date-transactions last :money last parse-float))

(defn to-amounts
  [date-transactions]
  (map #(-> % :money first parse-float) date-transactions))

(defn classify-transaction-date-group
  "take a list of transactions and a starting balance "
  [[starting-balance date-transactions]]
  (let [ending-balance (to-ending-balance date-transactions)
        transaction-amounts (to-amounts date-transactions)
        coefficients (get-valid-coefficients starting-balance ending-balance transaction-amounts)]
    (map set-transaction-sign date-transactions coefficients)))

(def transaction-display-keys
  [:date :withdrawal-or-deposit :amount])

(defn money-to-string
  [amount]
  (let [precise-amount (format "%.2f" amount)
        str-amount (format "%9s" precise-amount)]
    str-amount))

(defn money-to-signed-string
  [withdrawal-or-deposit amount]
  (let [precise-amount (format "%.2f" amount)
        signed-amount (if (= :withdrawal withdrawal-or-deposit)
                        (str "(" precise-amount ")")
                        (str precise-amount " "))
        str-amount (format "%9s" signed-amount)]
    str-amount))

(defn transaction-to-string
  [{:keys [date withdrawal-or-deposit amount description post-balance]}]
  (let [amount-str (money-to-signed-string withdrawal-or-deposit amount)
        balance-str (money-to-string post-balance)]
    (str date u/tab amount-str u/tab balance-str u/tab description)))

(defn fold-classified-transaction
  [[prior-balance finalized-transactions] transaction]
  (let [amount (parse-money (first (get transaction :money)))
        withdrawal (= :withdrawal (:withdrawal-or-deposit transaction))
        signed-delta (if withdrawal (- amount) amount)
        post-balance (+ prior-balance signed-delta)
        new-transaction (-> transaction
                            (dissoc :money)
                            (assoc :amount amount)
                            (assoc :post-balance post-balance))]
    [post-balance (conj finalized-transactions new-transaction)]))

(defn clean-classified-transactions
  [start-balance transactions]
  (second (reduce fold-classified-transaction
                  [start-balance []]
                  transactions)))

(defn fill-transactions
  [activity raw-transactions]
  (let [start-balance (parse-money (get-in activity [:account-balance :start :amount]))]
    (->> raw-transactions
         (group-transactions start-balance)
         (map classify-transaction-date-group)
         flatten
         (clean-classified-transactions start-balance)))) ;; (map transaction-to-string)

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
        transactions (fill-transactions activity raw-transactions)]
    transactions))

(defn fold-section-into-structured
  [[remaining-text processed] {:keys [section-name next-start optional]}]
  (if (and next-start
           (not (.contains remaining-text next-start)))
    (if optional
      [remaining-text (assoc processed section-name nil)]
      (throw (Exception. (str "Could not find required section " section-name " starting with " next-start))))
    (let [text-length (count remaining-text)
          index-of-remainder (if next-start
                               (s/index-of remaining-text next-start)
                               (inc text-length))
          no-more-remainder (if index-of-remainder
                              (> index-of-remainder text-length)
                              true)
          main-text (subs remaining-text 0 (dec index-of-remainder))
          new-remainder (if no-more-remainder nil (subs remaining-text index-of-remainder))
          new-processed (assoc processed section-name main-text)]
      [new-remainder new-processed])))

(defn segment-text
  "split text into labelled sections to simplify parse functions"
  [clean-text]
  (second
    (reduce
      fold-section-into-structured
      [clean-text {}]
      section-defs)))

(defn parse-statement-text
  [pdf-extracted-text]
  (let [segmented-text (-> pdf-extracted-text clean-raw-text segment-text)
        {activity-text :activity transactions-text :transactions} segmented-text
        activity (parse-activity activity-text)
        transactions (parse-transactions activity transactions-text)]
    {:activity activity :transactions transactions}))
