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