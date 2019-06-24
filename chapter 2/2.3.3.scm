;;; 2.59
(define (element-of-set? x set)
 (cond ((null? set) false)
       ((equal? x (car set)) true)
       (else (element-of-set? x (cdr set)))))


(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
        (cond ((or (null? set1) (null? set2)) '())
              ((element-of-set? (car set1) set2)
                (cons (car set1) (intersection-set (cdr set1) set2)))
              (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
          (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

;;; 2.60

;;; The functions are quite similar, there is no longer
;;; any reason to check whether or not an element
;;; is already in the set.
;;; element-of-set remains O(n)
;;; adjoin-set runs in O(1)
;;; intersection-set runs in O(n^2)
;;; union-set runs in O(n)
;;; Sets where we constantly add in repeated elements 
;;; may become slow in the duplicate representation.
;;; Intersection and set inspection heavy applications
;;; become a lot slower.
;;; In applications where we adjoin and make union
;;; sets very often the duplicate list has an advantage.

;;; 2.61

(define (adjoin-set x set)
 (cond ((null? set) (append set (list x)))
       ((< x (car set)) (cons x set))
       ((= x (car set)) set)
       (else (cons (car set) (adjoin-set x (cdr set))))))

;;; 2.62

(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        (else 
          (let ((x1 (car set1)) (x2 (car set2)))
               (cond ((= x1 x2)
                      (cons x1 (union-set (cdr set1) (cdr set2))))
                     ((< x1 x2)
                      (cons x1 (union-set (cdr set1) set2)))
                     ((< x2 x1)
                      (cons x2 (union-set set1 (cdr set2)))))))))

;;; 2.63

;;; a The two trees should produce the same result











