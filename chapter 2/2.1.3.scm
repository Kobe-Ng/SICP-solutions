;;; 2.4

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

;;; 2.5
(define (divides? a b)
  (= (remainder b a) 0))

(define (cons-2 a b)
  (* (expt 2 a) (expt 3 b)))

;;; Check how many times a value 'a' is multiplied into a number n
(define (extract-expt a n)
  (define (iter a n result)
    (if (divides? a n)
        (iter (/ n a) (+ 1 result))
        result))
  (iter a n 0))

(define (car-2 z)
  (extract-expt 2 z))

(define (cdr-2 z)
  (extract-expt 3 z))

