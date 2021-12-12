; bb .\bux_entrance.clj -d 10 -p "vj; 5; 2021-09-25" -p "other; 0; 2021-09-25"

(ns bux-entrance
  (:require [cheshire.core :as json]
            [clojure.string :as st]
            [clojure.tools.cli :as cli])
  (:import (java.time LocalDate)
           (java.time.format DateTimeFormatter)))

(defn dates
  [start]
  (iterate (fn [date] (.plusDays date 1)) start))

(defn format-dates
  [days]
  (let [formatter (DateTimeFormatter/ofPattern "dd.MM.yyyy")]
    (reduce-kv (fn [coll key val]
                 (assoc coll key (map (fn [x] (.format formatter x)) (flatten val)))) {} days)))

(defn find-common
  [people]
  (loop [data (frequencies (flatten (vals people)))
         max (apply max-key val data)
         res '()]
    (if (not= (count people) (val max))
      res
      (let [date (key max)
            dis (dissoc data date)]
        (recur dis (apply max-key val dis) (conj res date))))))

(def cli-options
  [["-p" "--people PEOPLE" "People, partition and start of weekend ex: senya; 5; 2021-9-25"
    :default {}
    :multi true
    :update-fn conj
    :parse-fn (fn [str]
                (let [split (st/split str #"; ")
                      name (keyword (first split))
                      parts (second split)
                      start-day (LocalDate/parse (nth split 2))]
                  {name
                   (cond
                     (= "5" parts) (fn [days]
                                     (partition 2 7 (take days (dates start-day))))
                     ;todo 3\2 & 3\1
                     ;and try find a way to not do func for every case
                     (= "2" parts) (fn [days]
                                     (take-nth 2 (partition 2 2 (take days (dates start-day)))))
                     :default (fn [days]
                                (take days (dates start-day))))}))]
   ["-d" "--days DAYS" "amount of days"
    :default 10
    :parse-fn #(Long/parseLong %)]])

(defn- set-days
  [days people]
  (reduce-kv (fn [map key val-func]
               (assoc map key (val-func days))) {} people))

(as->
  (:options (cli/parse-opts *command-line-args* cli-options)) data
  (set-days (:days data) (:people data))
  (format-dates data)
  (assoc data :max (sort (find-common data)))
  (print (json/encode data)))
