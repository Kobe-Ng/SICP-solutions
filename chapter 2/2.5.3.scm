;;; 2.87

 (define (zero-poly? poly) 
    (empty-termlist? (term-list poly))) 

(put 'zero? 'polynomial
  (zero? poly))

;;; 2.88

 (define (negate-terms termlist) 
    (map  (lambda (t) (make-term (order t) (- (coeff t)))) termlist)) 

(define (subtract-poly p1 p2)
  (add-poly L1 (make-poly (variable p2) (negate-terms (term-list p2)))))