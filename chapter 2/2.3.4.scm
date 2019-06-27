;;; 2.67

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))


(define (make-code-tree left right)
 (list left
       right
       (append (symbols left) (symbols right))
       (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
 (if (leaf? tree)
     (weight-leaf tree)
     (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits) '()
        (let ((next-branch
              (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
(decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define sample-tree (make-code-tree (make-leaf 'A 4)
                                    (make-code-tree
                                      (make-leaf 'B 2)
                                      (make-code-tree
                                        (make-leaf 'D 1)
                                        (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)
;;; a d a b b c a


; 2.68

(define (encode message tree)
 (if (null? message)
     '()
     (append (encode-symbol (car message) tree)
             (encode (cdr message) tree))))

(define (encode-symbol message tree)
  (define (iter message tree bit-list)
    (if (leaf? tree)
        bit-list
        (cond ((contains message (symbols (left-branch tree))) 
                (iter message
                     (left-branch tree)
                     (append bit-list '(0))))
              ((contains message (symbols (right-branch tree)))
                (iter message
                     (right-branch tree)
                     (append bit-list '(1))))
              (else (error "bad message: ENCODE-SYMBOL" message)))))
  (iter message tree '()))

(define (contains x list)
    (cond 
        ((null? list) #f)
        ((eq? x (car list)) #t)
        (else (contains x (cdr list)))))

(encode '(a d a b b c a) sample-tree)


;;; 2.69
(define (adjoin-set x set)
 (cond ((null? set) (list x))
       ((< (weight x) (weight (car set))) (cons x set))
       (else (cons (car set)
                   (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
 (if (null? pairs)
     '()
     (let ((pair (car pairs)))
          (adjoin-set (make-leaf (car pair)   ; symbol
                                 (cadr pair)) ; frequency
                      (make-leaf-set (cdr pairs))))))

(define (successive-merge leaves)
  (if (null? (cdr leaves))
      (car leaves)
      (successive-merge 
        (adjoin-set (make-code-tree (car leaves) (cadr leaves))
                    (cddr leaves)))))

(define (generate-huffman-tree pairs)
 (successive-merge (make-leaf-set pairs)))

;;; 2.70

(define tree-270 (generate-huffman-tree '((A 2)
                                          (GET 2)
                                          (SHA 3)
                                          (WAH 1)
                                          (BOOM 1)
                                          (JOB 2)
                                          (NA 16)
                                          (YIP 9))))

(encode 
  '(Get a job
    Sha na na na na na na na na
    Get a job
    Sha na na na na na na na na
    Wah yip yip yip yip yip yip yip yip yip
    Sha boom)
  tree-270)

;;; 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 
;;; 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0
;;; 0 0 0 0 0 0 0 0 1 1 0 1 1 1 0 1 0 1 0 1 0 1
;;; 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 0
;;; 84 bits
;;; 3*36 = 108 bits for a fix length code

;;; 2.71
;;; Tree sketched on paper
;;; most freqent symbol requires 1 bit
;;; least frequent symbol requires n-1 bit

;;; 2.72

;;; The least amount of steps it takes is n
;;; The most is n + n-1 + n-2 + n-3....
;;; O(nlogn) 
;;; max time is O(n^2)
;;; min time is O(n)







