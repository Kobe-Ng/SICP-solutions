;;; Useful functions from the chapter

(define (sum term a next b)
 	(if (> a b)
  		0
  		(+ (term a)
  			(sum term (next a) next b))))

(define (cube x)
 	(* x x x))

(define (inc n)
 	(+ n 1))

(define (sum-cubes a b)
 	(sum cube a inc b))

;;; 1.29
;;; The trick here is to use f to create a new function that 
;;; calculates y from just the i in y_i. Then iterate over i's from
;;; 0 to n. Iterating from a to b might be possible,
;;; but a solution with this method is hard to think of.
(define (integral f a b n)
	(let ((h (/ (- b a) n)))
		(define (y i)
		 	(f (+ a (* h i))))
		(define (new-f i)
    		(* 
        		(cond ((= 0 i) 1)
        			((= n i) 1)
        			((even? i) 2)
        			(else 4)) 
				(y i)))
		(* 
			(/ 
				h
				3)
			(sum new-f 0 inc n))))

;;;1.30
(define (iterative-sum sum term a next b)
 	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (+ result (term a))))
	(iter a 0)))

;;; 1.31a
(define (product term a next b)
 	(if (> a b)
  		1
  		(* (term a)
  			(product term (next a) next b))))


