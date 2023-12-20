;propotion
;x = 1 200 = 2 			|	 x = 2 * 3 / 4
;100 = x 200 = 2 	|	 x = 4 * 1 / 3
;100 = 1 x = 2 			|	 x = 4 * 1 / 2
;100 = 1 200 = x 	|	 x = 2 * 3 / 1
;50000 = 17558 (+ 13673.61 5.01 250.45) = x | x = acc * 2 / 1
(defn- find-x-idx [arg]
	(first (keep-indexed #(if (= "x" %2) %1) arg)))

(defn- calc [a b c]
	(/ (* (load-string a) (load-string b)) (load-string c)))

(let [equations (remove #(= %1 "=") *command-line-args*)
						x-idx (find-x-idx equations)
						formula (get [(fn [[_ a b c]] (calc a b c)) 
																				(fn [[a _ b c]] (calc c a b)) 
																				(fn [[a b _ c]] (calc c a b)) 
																				(fn [[a b c _]] (calc c b a))] x-idx)]
	(println (formula equations)))
