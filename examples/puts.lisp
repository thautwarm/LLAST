(suite
(decl puts [(ptr i8)] i32)
(def g [(a i32) (b i32)] i32
	 (- (+ a b) 222))
(def main[] i32
	(suite
		(puts (bitcast (store (alloca (arr 5 i8)) "123")  (ptr i8)))
		0
	)))
