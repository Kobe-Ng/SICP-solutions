;;; 2.2

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-line point1 point2)
  (cons point1 point2))

(define (start-segment line)
  (car line))

(define (end-segment line)
  (cdr line))

(define (midpoint-segment line)
  (make-point
  	(/ 
  	  (+ 
  		(x-point (start-segment line)) 
  		(x-point (end-segment line))) 
  	 2)
    (/ 
      (+ 
        (y-point (start-segment line))
    	(y-point (end-segment line)))
     2)))


;;; Skipping 2.3 because I have done rectangles a thousand times. 
;;; In general chapter 1 was interesting from the beginning as I had not done much recursion,
;;; but I have done data representation, so the start of this chapter is boring.
;;; You can represent rectangles with a point, width, and height; or with two points on opposite corners.