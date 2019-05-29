;;; Useful functions from the textbook. Often used for testing
(define (cube x)
  (* x x x))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess) 
  (fixed-point (newton-transform g) guess))

(define (inc x)
  (+ x 1))

(define (identity x)
  x)

;;; 1.40
(define (cubic a b c)
  (lambda (x) (+ 
  				(cube x) 
  				(* a (square x)) 
  				(* b x) 
  				c)))

;;; 1.41
(define (double f)
  (lambda (x) (f (f x))))

;; "(((double (double double)) inc) 5) = 21

;;; 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;;; 1.43
(define (repeated f n)
  (if (= n 1)
    (lambda (x) (f x))
    (lambda (x) ((compose f (repeated f (- n 1))) x))))

;;; 1.44


  