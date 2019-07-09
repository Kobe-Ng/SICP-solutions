;;; 2.87

 (define (zero-poly? poly) 
    (empty-termlist? (term-list poly))) 

(put 'zero? 'polynomial
  (zero? poly))