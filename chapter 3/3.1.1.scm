;;; 3.1

(define (accumulator initial)
  (let ((sum initial)) 
    (lambda (n) 
      (begin (set! initial (+ n initial))
              initial))))