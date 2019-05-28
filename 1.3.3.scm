;;; Useful functions from the chapter
(define tolerance 0.00001)
(define (fixed-point f first-guess)
	(define (close-enough? v1 v2) 
		(< (abs (- v1 v2))
			tolerance)) 
	(define (try guess)
		(let ((next (f guess)))
			(if (close-enough? guess next)
				next
          		(try next))))
  	(try first-guess))

;;; 1.35

(fixed-point 
	(lambda (x) (+ 1 
				(/ 1 x))) 1)

;;; 1.36
(define (fixed-point-p f first-guess)
	(define (close-enough? v1 v2) 
		(< (abs (- v1 v2))
			tolerance)) 
	(define (try guess)
		(let ((next (f guess)))
			(newline)
			(display guess)
			(if (close-enough? guess next)
				next
          		(try next))))
  	(try first-guess))

;;; This method takes 34 steps to terminate
(fixed-point-p (lambda (x) (/ (log 1000) (log x)))
				2)

(define (average a b)
	(/ (+ a b) 2))

;;; Adding average damping reduces the method to 9 steps.
(fixed-point-p (lambda (x) (average x (/ (log 1000) (log x)))) 2)

;;; 1.37
(define (cont-frac n d k)
	(define (cont-frac-helper n d i k)
		(/ (n i) 
			(+ (d i)
			(if (= i k)
				0
				(cont-frac-helper n d (+ 1 i) k)))))
	(cont-frac-helper n d 1 k))

(/ 1 (cont-frac 
	(lambda (i) 1.0) 
	(lambda (i) 1.0)
	50))

;;; 1.37b
(define (iterative-cont-frac n d k)
	(define (iterative-cont-frac-helper result n d i k)
		(if (= i 0)
			result
			(iterative-cont-frac-helper (/ (n i) (+ (d i) result)) n d (- i 1) k)))
	(iterative-cont-frac-helper (/ (n k) (d k)) n d (- k 1) k))

(/ 1 (iterative-cont-frac 
	(lambda (i) 1.0) 
	(lambda (i) 1.0)
	50))
