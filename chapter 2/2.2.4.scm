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