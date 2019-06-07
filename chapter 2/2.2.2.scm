;;; Useful functions from the chapter

(define (count-leaves x)
 (cond ((null? x) 0)
    ((not (pair? x)) 1)
    (else (+ (count-leaves (car x))
             (count-leaves (cdr x))))))

(define (make-branch length structure) 
  (list length structure))


(define (make-mobile left right)
 (list left right))

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

;;; 2.29b

(define (total-weight mobile)
  (define (iter branch)
    (if (not (list? (branch-structure branch)))
      (branch-structure branch)
      (+ (iter (right-branch branch))
         (iter (left-branch branch)))))
  (iter mobile))

;;; 2.29c

;;; Check the torque of current mobile, then check left and right branches recursively.
;;; if the left or right branch happens to be a number, just pass it off as true.
;;; (This is necessary because we can't really encode what we pass down. Everything must
;;; work on both branches, mobiles, and numbers when we hit the terminus. If this were typed,
;;; I would imagine that the function would become incredibly long though.)
(define (balanced? mobile)
  (if (number? mobile)
      true
  (and (= (* (total-weight (branch-structure (left-branch mobile)))
             (branch-length (left-branch mobile))
          (* (total-weight (branch-structure (right-branch mobile)))
             (branch-length (right-branch mobile)))))
       (balanced? (branch-structure (left-branch mobile)))
       (balanced? (branch-structure (right-branch mobile))))))

;;; 2.29d
;;; Certain checks within the functions should be changed 
;;; eg. checking if not list? changes to pair. This should probably be
;;; changed to a function like mobile? which checks if an argument matches
;;; the internal representation. Then we just have to change the selectors.

