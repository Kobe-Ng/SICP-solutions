;;; useful functions from the chapter

(define (length items)
  (define (length-iter a count)
    (if (null? a) count
                 (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))


(define (list-ref items n) 
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(define (map proc items) 
  (if (null? items)
    '()
    (cons (proc (car items))
      (map proc (cdr items)))))

;;; 2.17

(define (last-pair items)
  (list (list-ref items (- (length items) 1))))

;;; 2.18
(define (reverse items)
  (define (reverse-iter count list1)
    (if (= (length items) count)
      list1
      (reverse-iter (+ count 1) (cons (list-ref items count) list1))))
  (reverse-iter 0 '()))


;;; 2.19

(define (cc amount coin-values)
 (cond ((= amount 0) 1)
       ((or (< amount 0) (no-more? coin-values)) 0) (else
          (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))


(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

;;; 2.20

(define (same-parity first . rest)
  (define (same-parity a b)
    ; even + even, or odd + odd = even, odd + even = odd
    (even? (+ a b)))
  (define (iter count list1)
    (if (= (length rest) count)
      list1
      (if (same-parity first (list-ref rest count))
          (iter (+ count 1) (append list1 (list (list-ref rest count))))
          (iter (+ count 1) list1))))
  (cons first (iter 0 '())))

;;; 2.21

(define (square x)
  (* x x))

(define (square-list items) 
  (if (null? items)
    '()
    (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list-map items)
  (map (lambda (x) (* x x)) items))
