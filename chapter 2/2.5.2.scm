;;; 2.81

;;; a

;;; We get an infinite loop where we endlessly try to
;;; coerce a type into itself

;;; b

;;; There may be an operation defined 
;;; a type or two up the tower we never reach
;;; if we coerce into the same type.
;;; Something should be done (assuming no inheritance).

;;; c
;;; This doesn't really solve the problem I mentioned.
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (equal? type1 type2)
                    (error "No method for these types"
                           (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                              (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                              (apply-generic op a1 (t2->t1 a2)))
                            (else
                              (error "No method for these types"
                                     (list op type-tags)))))))
              (error "No method for these types"
(list op type-tags)))))))

;;; 2.82

;;; wow this question is tough

;;; 2.83

(define (raise x)
  (apply-generic 'raise x))

(put 'raise 'integer
  (lambda (x) (tag (make-rat x 1))))

 (put 'raise 'rational 
          (lambda (x) (tag (make-real (/ (numer x) (denom x)))))) 
  
 ;; add into real package 
 (put 'raise 'real 
          (lambda (x) (tag (make-from-real-imag x 0))))

;;; 2.84

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((a1 (car args))
                (a2 (cadr args)))
                (cond ((raise-up a1 a2)
                       (apply-generic op (raise-up a1 a2) a2))
                      ((raise-up a2 a1)
                       (apply-generic op (raise-up a2 a1) a1))
                      (else
                        (error "No method for these types"))))))))

(define (raise-up from to)
  (let ((type-from (type-tag from))
        (type-to (type-tag to)))
    (if (equal? type-from type-to)
        from 
        (let ((upper) (raise from))
          (if upper
              (raise-to upper to)
              #f))))) 

