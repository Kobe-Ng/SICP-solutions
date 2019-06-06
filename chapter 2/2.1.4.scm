;;; Functions from the chapter

(define (make-interval a b) (cons a b))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))



;;; 2.7

(define (upper-bound z)
  (cdr z))

(define (lower-bound z)
  (car z))

;;; 2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
  				       (- (upper-bound x) (lower-bound y))))

;;; 2.9
;;; Do I need a formal mathematical proof? With addition
;;; the for the new lower bound is center1 - width1 + center2 - width2.
;;; The new upper bound is center1 + width1 + center2 + width2
;;; So the new width is (upper bound - lower bound)/2 which is width1+width2.

;;; An interval spanning 0 to 0 multiplied by an interval spanning 1 to 2
;;; will have a width of 0. However, an interval spanning 3 to 3 multiplied
;;; by an interval spanning 1 to 2 has a width of 3. The input intervals
;;; have the same widths, but the outputs have different widths, so the 
;;; width of the output function depends on more than the width of the input functions.

;;; 2.10

(define (div-interval x y) 
  (define (same-signs? a b)
    (if (> (* a b) 0)
        #t 
        #f))
  (if (same-signs? (lower-bound y) (upper-bound y))
    (mul-interval
      x
      (make-interval (/ 1.0 (upper-bound y))
                     (/ 1.0 (lower-bound y))))
    (display "error: dividing an interval that spans 0")
  ))

;;; 2.11 Skipped because it seems more tedious than instructional

;;; 2.12

(define (make-center-percent center percent)
  (make-interval (- center (* center (/ percent 100.0))) (+ center (* center (/ percent 100.0)))))

(define (percent interval)
  (* 100 (/ (width interval) (center interval))))

;;; 2.13

;;; Every value is positive so the formula for
;;; the result of multiplication is easy to write down
;;; c represents the centre, p is the percent error.
;;; 1 and 2 denote if it's the first interval or second.
;;; ((c1-p1*c1)*(c2-p2*c2),(c1+p1*c1)*(c2+p2*c2))
;;; = (c1c2-c1c2p2-c2c1p1+c1c2p1p2, c1c2+c1c2p2+c2c1p1+c1c2p1p2)

;;; We now want to calculate the percent of this interval:
;;; width/center.
;;; I wrote simplified this on a separate sheet of paper.
;;; Assuming p1*p2 vanishes (percents are small),
;;; p3 = p1+p2

;;; 2.14

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
       (div-interval
          one (add-interval (div-interval one r1)
                            (div-interval one r2)))))

;;; for r1 = 1000 +/- 1%, r2 = 2000+/- 2%,
;;; par1 -> (646.9306930693069 . 686.936026936027)
;;; par2 -> (660. . 673.3333333333334)

;;; 2.15

;;; It doesn't matter which function provides smaller bounds,
;;; What matters is which procedure provides an answer
;;; that is correct. i.e, what answers:
;;; if a circuit is made with these resistors,
;;; what effective resistance range do I actually observe? 

;;; 2.16

;;; Equivalent expressions lead to different results
;;; because the inner procedures used are different.
;;; The interval arithmetic we have formulated does not 
;;; have the properties of a field. We could try
;;; and reformulate it so that it does (I have no idea
;;; if this is possible, or how to go about it). 
;;; One option is to throw out the idea interval arithmetic, 
;;; and reformulate it in terms of centre-error arithmetic.
;;; Center arithmetic is done normally, and error is calculated
;;; by normal error calculation methods. I think that has
;;; the structure of a ring.