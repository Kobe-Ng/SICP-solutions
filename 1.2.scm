;;;1.11
(define (f n)
	(cond ((= n 1) 1)
		  ((= n 2) 2)
		  ((= n 3) 3)
		  (else (+ (f (- n 1)) 
		  			(* 2 (f (- n 2)))
		  			(* 3 (f (- n 3)))))))

(define (iterative-f n)
	(f-iter 3 2 1 3 n))

(define (f-iter f-1 f-2 f-3 count max-count)
	(cond ((= max-count 1) 1)
		  ((= max-count 2) 2)
		  ((= max-count 3) 3)
		  (else	
				(if (= count max-count)
					f-1
					(f-iter (+ f-1 (* 2 f-2) (* 3 f-3)) f-1 f-2 (+ count 1) max-count)))))

;;;1.12
(define (Pascals-triangle row position)
	(cond ((= position 1) 1)
		  ((= position (+ row 1)) 1)
		  (else
		  		(+ 
		  			(Pascals-triangle (- row 1) position)
		  			(Pascals-triangle(- row 1) (- position 1))))))