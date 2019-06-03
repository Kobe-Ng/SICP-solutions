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

