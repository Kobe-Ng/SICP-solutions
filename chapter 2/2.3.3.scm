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

;;; b Both implementations should reach the same amount of nodes,
;;; but I'm not sure how the append will factor in here.
 

 ;;; 2.64

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
 (if (= n 0)
  (cons '() elts)
  (let ((left-size (quotient (- n 1) 2)))
       (let ((left-result
             (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
         (let ((this-entry (car non-left-elts))
               (right-result
                (partial-tree
                 (cdr non-left-elts)
                 right-size)))
                  (let ((right-tree (car right-result))
                        (remaining-elts
                         (cdr right-result)))
                    (cons (make-tree this-entry
                                     left-tree
                                     right-tree)
                          remaining-elts))))))))

;;; The tree draw from 1 3 5 7 9 11 is 

;;;          5
;;;         / \
;;;        1   9
;;;        \   / \
;;;        3   7 11
;;; The algorithm splits the problem into a median entry,
;;; values smaller than the median, and values larger than the
;;; median. It then recursively calls itself on the larger
;;; and smaller values forming smaller trees.

;;; b
;;; O(n)

;;; 2.65

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
   (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                              (right-branch tree)
                              result-list)))))
  (copy-to-list tree '()))

(define (union-set-tree set1 set2)
  (list->tree (union-set 
                (tree->list-2 set1)
                (tree->list-2 set2))))

(define (intersection-set-tree set1 set2)
  (list->tree (intersection-set 
                (tree->list-2 set1)
                (tree->list-2 set2))))

;;; 2.66
 (define (look-up given-key set-of-records)
   (cond ((null? set-of-records) false)
         ((= given-key (key (entry set-of-records))) (entry set-of-records))
         ((< given-key (key (entry set-of-records))
            (look-up given-key (left-branch set-of-records)))
         ((> given-key (entry set-of-records))
            (look-up given-key (right-branch set-of-records)))))







