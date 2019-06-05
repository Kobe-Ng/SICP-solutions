;;; Useful functions from the chapter

(define (count-leaves x)
 (cond ((null? x) 0)
    ((not (pair? x)) 1)
    (else (+ (count-leaves (car x))
             (count-leaves (cdr x))))))

;;; 2.24

;;; (count-leaves (list 1 (list 2 (list 3 4)))) results in 4
;;;      / \
;;;     1  / \
;;;       2  / \
;;;         3  4
;;; Box diagram not included
