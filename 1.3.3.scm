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

;;; takes 34 steps
(fixed-point-p (lambda (x) (/ (log 1000) (log x)))
				2)

(define (average a b)
	(/ (+ a b) 2))

;;; takes 9 steps with average damping
(fixed-point-p (lambda (x) (average x (/ (log 1000) (log x)))) 2)
