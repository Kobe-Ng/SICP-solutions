;;; Useful functions from the chapter


(define (accumulate op initial sequence) 
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

 (define (enumerate-tree tree) 
   (cond ((null? tree) '()) 
         ((not (pair? tree)) (list tree)) 
         (else (append (enumerate-tree (car tree)) 
                       (enumerate-tree (cdr tree)))))) 

(define (dot-product v w)
      (accumulate + 0 (map* '() * v w)))

(define (fold-left op initial sequence) 
  (define (iter result rest)
    (if (null? rest) 
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))


(define (prime? n)
(= n (smallest-divisor n)))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
 (cond ((> (square test-divisor) n) n)
       ((divides? test-divisor n) test-divisor)
       (else (find-divisor n (+ test-divisor 1))))) 

(define (divides? a b) (= (remainder b a) 0))


;;; 2.33

(define (map-2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
(define (append seq1 seq2) 
  (accumulate cons seq2 seq1))
(define (length sequence)
 (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;;; 2.34

(define (horner-eval x coefficient-sequence) 
  (accumulate (lambda (this-coeff higher-terms) 
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;;; 2.35

 (define (count-leaves t) 
   (accumulate + 
               0 
               (map (lambda (x) 1)  
                    (enumerate-tree t)))) 

;;; 2.36

(define (accumulate-n op init seqs) 
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;;; 2.37

(define (matrix-*-vector m v) 
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n (lambda (x y) (cons x y)) '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x)
            (matrix-*-vector cols x)) m)))

;;; 2.38

(define fold-right accumulate)

(fold-right / 1 (list 1 2 3))      ; 3/2
(fold-left / 1 (list 1 2 3))       ; 1/6
(fold-right list '() (list 1 2 3)) ; (1 (2 (3 ())))
(fold-left list '() (list 1 2 3))  ; (((() 1) 2) 3)

;;; I'd imagine that the op must be commutative 
;;; ie (op a b) = (op b a)

;;; 2.39

;;; I feel bad since I just kind of trial and errored
;;; around combinations of appending lists and such until I got it.
;;; TODO: actually understand what's going on.
(define (reverse-right sequence)
   (fold-right (lambda (x y) (append y (list x))) '() sequence))
(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))


;;; 2.40

(define (enumerate-interval a b)
  (define (iter count acc)
    (if (> count b)
        acc
        (iter (+ 1 count) (append acc (list count)))))
  (iter a '()))

(define (unique-pairs n)
  (flatmap (lambda (i)
              (map (lambda (j) (list i j))
                   (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n) 
  (map make-pair-sum
    (filter prime-sum? (unique-pairs n))))

;;; 2.41

(define (unique-triples n)
  (flatmap (lambda (pair)
              (map (lambda (i) (append pair (list i)))
                   (enumerate-interval 1 (- (cadr pair) 1))))
           (unique-pairs n)))

;;; 2.42

(define (queens board-size) 
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                      (adjoin-position
                       new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (adjoin-position  new-row k rest-of-queens)
  (append rest-of-queens (list new-row k)))

(define (list-ref items n) 
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define (last-pair items)
  (list (list-ref items (- (length items) 1))))

(define (safe? k positions)
  (not (attacked-by positions (last-pair positions))))

;;; Define attacked by. A position is attacked 
;;; by the other positions if it is found in the list of attacked
;;; spots. To do this I need to generate a list of spots every
;;; queen attacks. That can be done by creating a function
;;; that returns the spots one queen attacks, and appending
;;; them together.


