(ns finance-parser.util
  (:gen-class))

(def banner "========================================================================")
(def nl "\n")
(def tab "\t")

(defn banner-print
  [value]
  (println (str banner nl value nl banner))
  value)

(defn header-print
  [header message]
  (banner-print header)
  (println message)
  message)

(defn header-print-list
  [header messages]
  (banner-print (str header " (" (count messages) ")") )
  (doseq [x messages]
    (println x))
  messages)

(defn header-dump
  [header message]
  (banner-print header)
  (println (pr-str message))
  message)

(defn header-dump-list
  [header messages]
  (banner-print (str header " (" (count messages) ")") )
  (doseq [x messages]
    (println (pr-str x)))
  messages)

(defn deep-merge [v & vs]
  (letfn [(rec-merge [v1 v2]
            (if (and (map? v1) (map? v2))
              (merge-with deep-merge v1 v2)
              v2))]
    (when (some identity vs)
      (reduce #(rec-merge %1 %2) v vs))))
