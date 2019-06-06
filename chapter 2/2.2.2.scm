;;; Useful functions from the chapter

(define (count-leaves x)
 (cond ((null? x) 0)
    ((not (pair? x)) 1)
    (else (+ (count-leaves (car x))
             (count-leaves (cdr x))))))

(define (make-branch length structure) 
  (list length structure))


(define (make-mobile left right
 (list left right)))

;;; 2.24

;;; (count-leaves (list 1 (list 2 (list 3 4)))) results in 4
;;;      / \
;;;     1  / \
;;;       2  / \
;;;         3  4
;;; Box diagram not included

;;; 2.25
;;; car cdr car cdr cdr
;;; car car
;;; cadr cadr cadr cadr cadr cadr

;;; 2.26

;;; (1 2 3 4 5 6)
;;; ((1 2 3) 4 5 6)
;;; ((1 2 3), (4 5 6))

;;; 2.27

(define (deep-reverse items)
  (define (iter items acc)
    (if (null? items)
        acc
        (if (list? (car items))
            (iter (cdr items) (append (list (deep-reverse (car items))) acc))
            (iter (cdr items) (append (list (car items)) acc)))))
(iter items '()))

;;; 2.28

(define (fringe tree)
  (define (iter items acc)
    (if (null? items)
      acc
      (if (list? (car items))
        (iter (cdr items) (append acc (fringe (car items))))
        (iter (cdr items) (append acc (list (car items)))))))
  (iter tree '()))

;;; 2.29a

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))


