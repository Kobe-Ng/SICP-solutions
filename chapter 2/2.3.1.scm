;;; 2.53

;;; (a b c)
;;; ((george))
;;; ((y1 y2))
;;; (y1 y2)
;;; #f
;;; #f
;;; (red shoes blue socks)

;;; 2.54

(define (equal? a b)
  (cond ((and (null? a) (null? b)) #t)
        ((not (eq? (car a) (car b))) #f)
        (else (equal? (cdr a) (cdr b)))))

;;; 2.55

;;; The first quote makes the following characters evaluate as symbols
;;; (quote (quote abracadabra)) = '(quote abracadabra) as a symbol
;;; car then takes the first element of the list, quote.