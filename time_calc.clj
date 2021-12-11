(ns time-calc
  (:import (java.time.format DateTimeFormatter)
           (java.time LocalDate LocalTime LocalDateTime Duration Period)
           (java.util.regex Pattern)
           (clojure.lang LazySeq)
           (java.time.temporal ChronoUnit)))

(defn- HH:mm? [string]
  (Pattern/matches "^([0-1]?[0-9]|2[0-3]):[0-5][0-9]$" string))

(defn- dd.MM.yyyy? [string]
  (Pattern/matches "^\\s*(3[01]|[12][0-9]|0?[1-9])\\.(1[012]|0?[1-9])\\.((?:19|20)\\d{2})\\s*$" string))

(defn- dd.MM.yyyy_HH:mm? [string]
  (Pattern/matches "^\\s*(3[01]|[12][0-9]|0?[1-9])\\.(1[012]|0?[1-9])\\.((?:19|20)\\d{2})_[012]{0,1}[0-9]:[0-6][0-9]\\s*$" string))

(defn- unit? [string]
  (some? (re-seq #"(\d+)([smhdMy])" string)))

(defn- parse-arg [arg]
  (cond
    (= "now" arg) (LocalDateTime/now)
    (HH:mm? arg) (LocalTime/parse arg)
    (dd.MM.yyyy? arg) (.atStartOfDay (LocalDate/parse arg (DateTimeFormatter/ofPattern "dd.MM.yyyy")))
    (dd.MM.yyyy_HH:mm? arg) (LocalDateTime/parse arg (DateTimeFormatter/ofPattern "dd.MM.yyyy_HH:mm"))
    (unit? arg) (as-> arg data
                      (re-seq #"(\d+)([smhdMy])" data)
                      (map rest data)
                      (map (partial map (fn [x] (let [num (re-find #"\d+" x)]
                                                  (if (nil? num)
                                                    x
                                                    (Long/parseLong x))))) data))))

(defn- resolve-operand [operand args]
  (cond
    (every? (fn [x] (instance? LocalTime x)) args) (cond
                                                     (= "-" operand) :time-minus)

    (and
      (some (fn [x] (instance? LocalTime x)) args)
      (some (fn [x] (instance? LazySeq x)) args)) (cond
                                                    (= "-" operand) :minus-time-unit
                                                    (= "+" operand) :plus-time-unit)

    (or
      (every? (fn [x] (instance? LocalDate x)) args)
      (every? (fn [x] (instance? LocalDateTime x)) args)) (cond
                                                            (= "-" operand) :minus-local-date-time)

    (or
      (and
        (some (fn [x] (instance? LocalDate x)) args)
        (some (fn [x] (instance? LazySeq x)) args))
      (and
        (some (fn [x] (instance? LocalDateTime x)) args)
        (some (fn [x] (instance? LazySeq x)) args))) (cond
                                                       (= "-" operand) :minus-local-date-unit
                                                       (= "+" operand) :plus-local-date-unit)))

(defn- parse-args [args]
  (loop [args args
         parsed-args []]
    (if-not (empty? args)
      (recur (rest args) (conj parsed-args (parse-arg (first args))))
      parsed-args)))

(defn- parse-command-line [command-line-args]
  (let [args (parse-args (rest command-line-args))]
    (reverse (conj args (resolve-operand (first command-line-args) args)))))

(defn- format-period
  ([duration] (let [hh (.toHoursPart duration)
                    min (.toMinutesPart duration)
                    sec (.toSecondsPart duration)]
                (format "%d:%02d:%02d" hh min sec)))
  ([period duration] (let [yy (.getYears period)
                           mm (.getMonths period)
                           dd (.getDays period)
                           hh (.toHoursPart duration)
                           min (.toMinutesPart duration)
                           sec (.toSecondsPart duration)]
                       (format "%02d %02d %02d %d:%02d:%02d" yy mm dd hh min sec))))

(let [args (parse-command-line *command-line-args*)]
  (apply ({:time-minus            (fn [second-arg first-arg]
                                    (format-period (Duration/between second-arg first-arg)))
           :minus-time-unit       (fn [f s]
                                    (-> (DateTimeFormatter/ofPattern "HH:mm")
                                        (.format (reduce (fn [date [val unit]]
                                                           (.minus date val ({"s" ChronoUnit/SECONDS
                                                                              "m" ChronoUnit/MINUTES
                                                                              "h" ChronoUnit/HOURS} unit))) s f))))
           :plus-time-unit        (fn [f s]
                                    (-> (DateTimeFormatter/ofPattern "HH:mm")
                                        (.format (reduce (fn [date [val unit]]
                                                           (.plus date val ({"s" ChronoUnit/SECONDS
                                                                             "m" ChronoUnit/MINUTES
                                                                             "h" ChronoUnit/HOURS} unit))) s f))))
           :minus-local-date-time (fn [f s]
                                    (format-period (Period/between ^LocalDate (.toLocalDate f) ^LocalDate (.toLocalDate s))
                                                   (Duration/between f s)))
           :minus-local-date-unit (fn [f s]
                                    (-> (DateTimeFormatter/ofPattern "dd.MM.yyyy HH:mm")
                                        (.format (reduce (fn [date [val unit]]
                                                           (.minus date val ({"s" ChronoUnit/SECONDS
                                                                              "m" ChronoUnit/MINUTES
                                                                              "h" ChronoUnit/HOURS
                                                                              "d" ChronoUnit/DAYS
                                                                              "M" ChronoUnit/MONTHS
                                                                              "y" ChronoUnit/YEARS} unit))) s f))))
           :plus-local-date-unit  (fn [f s]
                                    (-> (DateTimeFormatter/ofPattern "dd.MM.yyyy HH:mm")
                                        (.format (reduce (fn [date [val unit]]
                                                           (.plus date val ({"s" ChronoUnit/SECONDS
                                                                             "m" ChronoUnit/MINUTES
                                                                             "h" ChronoUnit/HOURS
                                                                             "d" ChronoUnit/DAYS
                                                                             "M" ChronoUnit/MONTHS
                                                                             "y" ChronoUnit/YEARS} unit))) s f))))} (first args)) (rest args)))