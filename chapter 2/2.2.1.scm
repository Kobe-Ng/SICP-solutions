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

;;; 2.17

(define (last-pair items)
  (list (list-ref items (- (length items) 1))))