;;; Useful functions from the chapter


(define (accumulate op initial sequence) 
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;;; 2.33

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
(define (append seq1 seq2) 
  (accumulate cons seq2 seq1))
(define (length sequence)
 (accumulate (lambda (x y) (+ 1 y)) 0 sequence))