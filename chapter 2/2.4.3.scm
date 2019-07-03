;;; 2.73a

;;; Numbers and variables cannot be extracted into further operators and operands

;;; 2.73 b


(define (install-sum-package)
  (define (make-sum a1 a2) 
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (addend operands) (car s))
  (define (augend operands) (cadr s))
  (define (deriv-sum operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))
  (put 'deriv '(sum) deriv-sum)
  'done)

(define (install-product-package)
  (define (make-product m1 m2) 
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (multiplier operands) (car p))
  (define (multiplicand operands) (cadr p))
  (define (deriv-product operands var)
    (make-sum
      (make-product (multiplier operands)
                    (deriv (multiplicand operands) var))
      (make-product (deriv (multiplier operands) var)
                    (multiplicand operands))))
  (put 'deriv '(product) deriv-product)
  'done)

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
(and (number? exp) (= exp num)))

;;; c

(define (install-exponentiation-package)
  (define (make-exponentiation b e)
    (cond ((=number? e 0) 1)
          ((=number? e 1) b)
          ((and (number? b) (number? e)) (expt b e))
          (else (list '** b e))))
  (define (base operands) (car operands))
  (define (exponent operands) (cadr operands))
  (define (deriv-exponentiation operands var)
    (make-product
            (make-product (exponent exp)
                          (make-exponentiation (base exp)
                                               (make-sum
                                                 (exponent exp)
                                                 '-1)))
            (deriv (base exp) var)))
  (put 'deriv '(exponentiation) deriv-exponentiation)
  'done)

;;; 2.74a

;;; a
(define (get-record division employee-name) 
  ((get division 'record) employee-name)) 

;;; b
(define (get-salary division record) 
  ((get division 'salary) record)) 
  
;;; c
(define (find-employee-record employee-name division-list) 
  (if (null? division-list) 
       #f 
       (or (get-record (car division-list) employee-name) 
           (find-employee-record employee-name (cdr division-list))))) 

;;; d
;;; skipped

;;; 2.75

(define (make-from-mag-ang r a)
 (define (dispatch op)
         (cond ((eq? op 'real-part) (* r (cos a)))
         ((eq? op 'imag-part) (* r (sin a)))
         ((eq? op 'magnitude) r) 
         ((eq? op 'angle) a)
         (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)

;;; 2.76

;;; explicit dispatch

;;; To add a new type every function must have the
;;; dispatch cond accomodate the new types.
;;; Every new operation is written on it's own
;;; no changes to previous types are necessary.

;;; data-directed style

;;; To add a new type all we have to do is add
;;; in the appropriate column (no changes to existing code).
;;; To add in a new operator we fill in the appropriate
;;; row (change existing code).

;;; message passing

;;; Adding a new operation requires updating every type
;;; Adding a new type is trivially easy.

;;; Message passing and data directed style are better
;;; if many new types are being added as adding types
;;; doesn't require one to jump around code.
;;; Explicit dispatch is superious if one is adding
;;; a lot of operations.










