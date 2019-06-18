;;; 2.44

(define (up-split painter n)
 (if (= n 0)
     painter
     (let ((smaller (up-split painter (- n 1))))
          (below painter (beside smaller smaller)))))

;;; 2.45

(define (split op1 op2)
  (lambda (painter n)
    ((if (= n 0)
          painter
         (let ((smaller (right-split painter (- n 1))))
              (beside painter (below smaller smaller)))))))



             