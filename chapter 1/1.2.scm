;;; 1.11
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

;;; 1.12
(define (Pascals-triangle row position)
	(cond ((= position 1) 1)
		  ((= position (+ row 1)) 1)
		  (else
		  		(+ 
		  			(Pascals-triangle (- row 1) position)
		  			(Pascals-triangle (- row 1) (- position 1))))))

;;; 1.16
(define (fast-expt b n)
	(fast-expt-iterative b n 1))

(define (fast-expt-iterative b n result)
	(cond((= n 0) result)
		 ((even? n) (fast-expt-iterative (* b b) (/ n 2) result))
		 (else (fast-expt-iterative b (- n 1) (* b result)))))

(define (even? n)
	(= (remainder n 2) 0))

;;; 1.17
(define (double x)
	(+ x x))

;; Could throw an error or something if x is not even but I don't think it's relevant
(define (halve x)
	(/ x 2))

(define (fast-* a b)
	(cond ((= b 1) a)
		((even? b) (fast-* (double a) (halve b)))
		(else (+ a (fast-* a (- b 1))))))

;;; 1.18
(define (fast-iterative-* a b)
	(fast-*-iter a b 0))

(define (fast-*-iter a b result)
	(cond ((= b 0) result)
		((even? b) (fast-*-iter (double a) (halve b) result))
		(else (fast-*-iter a (- b 1) (+ a result)))))

;;; 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
			(fib-iter a b
				(+ (* q q) (* p p)) ; compute p’
				(+ (* q q) (* 2 p q)) ; compute q’ 
				(/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))