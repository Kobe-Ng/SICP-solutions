;;; 2.7
(define (make-interval a b) (cons a b))

(define (upper-bound z)
  (cdr z))

(define (lower-bound z)
  (car z))

;;; 2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y) )
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