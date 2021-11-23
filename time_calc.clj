(ns time-calc
  (:import (java.time.format DateTimeFormatter)
           (java.time LocalDate LocalTime LocalDateTime Duration Period)
           (java.util.regex Pattern)))

(defn- HH:mm? [string]
  (Pattern/matches "^([0-1]?[0-9]|2[0-3]):[0-5][0-9]$" string))

(defn- dd.MM.yyyy? [string]
  (Pattern/matches "^\\s*(3[01]|[12][0-9]|0?[1-9])\\.(1[012]|0?[1-9])\\.((?:19|20)\\d{2})\\s*$" string))

(defn- dd.MM.yyyy_HH:mm? [string]
  (Pattern/matches "^\\s*(3[01]|[12][0-9]|0?[1-9])\\.(1[012]|0?[1-9])\\.((?:19|20)\\d{2})_[012]{0,1}[0-9]:[0-6][0-9]\\s*$" string))

(defn- parse-arg [arg]
  (cond
    (= "now" arg) (LocalDateTime/now)
    (HH:mm? arg) (LocalTime/parse arg)
    (dd.MM.yyyy? arg) (.atStartOfDay (LocalDate/parse arg (DateTimeFormatter/ofPattern "dd.MM.yyyy")))
    ;todo need auto time resolve
    (dd.MM.yyyy_HH:mm? arg) (LocalDateTime/parse arg (DateTimeFormatter/ofPattern "dd.MM.yyyy_HH:mm"))
    :default (Long/parseLong arg)))

(defn- resolve-operand [operand args]
  (cond
    (every? (fn [x] (instance? LocalTime x)) args) (cond
                                                     (= "-" operand) :time-minus)

    (and
      (some (fn [x] (instance? LocalTime x)) args)
      (some (fn [x] (instance? Long x)) args)) (cond
                                                 (= "-" operand) :minus-time-long
                                                 (= "+" operand) :plus-time-long)

    (or
      (every? (fn [x] (instance? LocalDate x)) args)
      (every? (fn [x] (instance? LocalDateTime x)) args)) (cond
                                                            (= "-" operand) :minus-local-date-time)

    (or
      (and
        (some (fn [x] (instance? LocalDate x)) args)
        (some (fn [x] (instance? Long x)) args))
      (and
        (some (fn [x] (instance? LocalDateTime x)) args)
        (some (fn [x] (instance? Long x)) args))) (cond
                                                    (= "-" operand) :minus-local-date-long
                                                    (= "+" operand) :plus-local-date-long)))

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
           :minus-time-long       (fn [f s]
                                    ;todo units
                                    (-> (DateTimeFormatter/ofPattern "HH:mm")
                                        (.format (.minusHours s f))))
           :plus-time-long        (fn [f s]
                                    ;todo units
                                    (-> (DateTimeFormatter/ofPattern "HH:mm")
                                        (.format (.plusHours s f))))
           :minus-local-date-time (fn [f s]
                                    (format-period (Period/between (.toLocalDate f) (.toLocalDate s))
                                                   (Duration/between f s)))
           :minus-local-date-long (fn [f s]
                                    ;todo units
                                    (-> (DateTimeFormatter/ofPattern "dd.MM.yyyy HH:mm")
                                        (.format (.minusDays s f))))
           :plus-local-date-long  (fn [f s]
                                    ;todo units
                                    (-> (DateTimeFormatter/ofPattern "dd.MM.yyyy HH:mm")
                                        (.format (.plusDays s f))))} (first args)) (rest args)))