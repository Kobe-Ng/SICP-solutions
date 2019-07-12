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

;;; 3.3

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
                     balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
          balance)
  (define (dispatch m pass)
    (cond ((not (eq? pass password)) 
              ; lambda to absorb the input so an error isn't thrown
              (lambda (x) '"incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                  m))))
dispatch)
