;bb calc-vals.clj rub 42000 kgs 40688.60 arsen '(+ 27236.40 5.45 272.34)' ambal '(+ 13618.20 5.01 250.45)'
(println *command-line-args*)
(let [data (zipmap (keep-indexed #(if (even? %1) (keyword %2)) *command-line-args*) 
																			(keep-indexed #(if (odd? %1) (load-string %2)) *command-line-args*))]
	(let [rub  (:rub data)
							kgs  (:kgs data)
							one  (/ kgs rub)
							data_with_rub (assoc data :arsen_rub (/ (:arsen data) one)
																																	:ambal_rub (/ (:ambal data) one))] 
							(println data_with_rub)))
;5044 долженe

