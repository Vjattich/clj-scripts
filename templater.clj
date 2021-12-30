; bb .\templater.clj C:\Users\sekta\Desktop\trash\test_tempalte.edn

;gets a edn input and multiplate one with another
;prints a tempaltes in console

;{:names ("ASD" "SDA" "DSA")
; :templates ("create or replace trigger TR_MONITORING_%1$s
;  after insert or update or delete
;   on %1$s
;   begin
;    merge into ORACLE_TABLE_CDC_MONITORING
;    using dual
;    on (TABLE_NAME = '%1$s')
;      when matched then
;      update set \"DATE\" = systimestamp
;      when not matched then
;      insert values ('%1$s', systimestamp);
;  end;")}

(ns templater
  (:require [clojure.edn :as edn]))

(defn- read-edn [arg]
  (edn/read-string (slurp arg)))

;todo pprint
(let [input (read-edn (first *command-line-args*))]
  (clojure.pprint/pprint
    (for [names (:names input)
          templates (:templates input)]
      (format templates names))))
