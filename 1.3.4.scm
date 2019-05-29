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
(define (smooth f)
  (lambda (x) 
    (let ((dx 0.0001))
      (/ 
        (+ (f (+ x dx))
           (f x)
           (f (- x dx))
        3)))))

(define (n-fold-smooth f n)
  (lambda (x) (((repeated smooth n) f) x)))

;;; I am skipping 1.45, it does not seem very interesting

;;; 1.46

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (lambda (guess) (iter guess)))

(define (sqrt x)
  (define (square x)
    (* x x))
  (define (average x y)
    (/ (+ x y) 2))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point f guess)
  (define (close-enough? v1 v2)
    (let ((tolerance 0.00001))
      (< (abs (- v1 v2)) tolerance)))
  (let ((good-enough? (lambda (x) (close-enough? x (f x))))
        (improve f))
((iterative-improve good-enough? improve) guess)))


  