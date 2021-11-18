(ns time-calc
  (:import (java.time.format DateTimeFormatter)
           (java.time LocalDate LocalTime LocalDateTime Duration Period)))

(defn hh:mm? [arg]
  (some? (re-matches #"^([0-1]?[0-9]|2[0-3]):[0-5][0-9]$" arg)))

(defn dd:mm:yy? [string]
  (some? (re-matches #"^\s*(3[01]|[12][0-9]|0?[1-9])\.(1[012]|0?[1-9])\.((?:19|20)\d{2})\s*$" string)))

(defn- parse-arg [arg]
  (cond
    (= "-" arg) :-
    (= "+" arg) :+
    (= "now" arg) (LocalDateTime/now)
    (hh:mm? arg) (LocalTime/parse arg)
    (dd:mm:yy? arg) (.atStartOfDay (LocalDate/parse arg (DateTimeFormatter/ofPattern "dd.MM.yyyy")))
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
  (println args)
  (apply ({:time-minus            (fn [second-arg first-arg]
                                    (format-period (Duration/between second-arg first-arg)))
           :minus-time-long       (fn [second-arg first-arg]
                                    ;todo units
                                    (.toString (.minusHours first-arg second-arg)))
           :plus-time-long        (fn [second-arg first-arg]
                                    ;todo units
                                    (.toString (.plusHours first-arg second-arg)))
           :minus-local-date-time (fn [second-arg first-arg]
                                    (format-period (Period/between (.toLocalDate second-arg) (.toLocalDate first-arg))
                                                   (Duration/between second-arg first-arg)))
           :minus-local-date-long (fn [second-arg first-arg]
                                    ;todo units
                                    (.toString (.minusDays first-arg second-arg)))
           :plus-local-date-long  (fn [second-arg first-arg]
                                    ;todo units
                                    (.toString (.plusDays first-arg second-arg)))} (first args)) (rest args)))