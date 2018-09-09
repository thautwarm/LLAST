(suite
(decl printf [(ptr i8) i32] i32)

(def g [(a i32) (b i32)] i32
	 (- (+ a b) 100))

(def main[] i32
	(suite
		(printf (bitcast (store (alloca (arr 5 i8)) "1%d")  (ptr i8)) (g 10 20) )
		0
	)))
