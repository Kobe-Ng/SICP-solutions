;;; 3.1

(define (accumulator initial)
  (let ((sum initial)) 
    (lambda (n) 
      (begin (set! initial (+ n initial))
              initial))))

;;; 3.2

(define (make-monitored f)
  (let ((counter 0))
    (define (reset-count)
      (begin (set! counter 0)
             counter))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls) counter)
            ((eq? m 'reset-count) (reset-count))
            (else (begin (set! counter (+ counter 1))
                         (f m)))))
dispatch))
