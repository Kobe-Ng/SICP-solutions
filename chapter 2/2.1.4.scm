;;; 2.7
(define (make-interval a b) (cons a b))

(define (upper-bound z)
  (car z))

(define (lower-bound z)
  (cdr z))

;;; 2.8

(define (sub-interval x y)
  (make-interval (- (upper-bound x) (lower-bound y))
  				 (- (lower-bound x) (upper-bound y))))

