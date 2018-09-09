(suite
(decl printf [(ptr i8) i32] i32)
(def sum [(len i32) (array (ptr i32))] i32 
	(suite
		(let x (alloca i32)
		 
		 (suite
		 (store x 0)
		 (let s (alloca i32)
		 	(suite
			(while (< (load x) len)
				(suite
				(store s (+ (load s) (load (gep array (load x)))))
				(store x (+ (load x) 1))))
			(load s)))))))

(def main [] i32
	(let sum 
			(sum 5 (let array (alloca (arr 5 i32)) 
			(suite
			(store array (arr 1 2 3 7 10))
			(bitcast array (ptr i32)))))
		(suite
			(printf 
				(bitcast (store (alloca (arr 10 i8)) "sum = %d")  (ptr i8)) sum)
		     0))))