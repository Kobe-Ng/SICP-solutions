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