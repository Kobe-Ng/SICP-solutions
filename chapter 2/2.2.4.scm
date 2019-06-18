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

;;; 2.46

(define (make-vect x y)
  (cons x y))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cdr vect))

(define (add-vect vect1 vect2)
  (make-vect (+ (xcor-vect vect1) (xcor-vect vect2))
             (+ (ycor-vect vect1) (ycor-vect vect2))))  

(define (sub-vect vect1 vect2)
  (make-vect (- (xcor-vect vect1) (xcor-vect vect2))
             (- (ycor-vect vect1) (ycor-vect vect2))))

(define (scale-vect c vect)
  (make-vect (* c (xcor-vect vect))
             (* c (ycor-vect vect))))  

;;; 2.47

(define (make-frame1 origin edge1 edge2) (list origin edge1 edge2))

(define (origin-1 frame)
  (car frame)) 

(define (edge1-1 frame)
  (cadr frame))

(define (edge2-1 frame)
  (cadr(cdr frame)))

(define (make-frame2 origin edge1 edge2) (cons origin (cons edge1 edge2)))

(define (origin-2 frame)
  (car frame)) 

(define (edge1-2 frame)
  (cadr frame))

(define (edge2-2 frame)
  (cdr (cdr frame)))

;;; 2.48

(define (make-seg from-origin segment)
  (cons from-origin segment))

(define (start-seg segment)
  (car segment))

(define (end-seg segment)
  (let ((coordinate (add-vect (car segment) (cdr segment))))
    (cons (xcor-vect coordinate) (ycor-vect coordinate))))




