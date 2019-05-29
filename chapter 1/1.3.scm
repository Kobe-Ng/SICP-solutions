;;; Useful functions from the book

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


(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor) (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
(else (find-divisor n (+ test-divisor 1))))) (define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
(= n (smallest-divisor n)))

(define (gcd a b) (if (= b 0)
      a
      (gcd b (remainder a b))))

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
(define (iterative-sum term a next b)
 	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (+ result (term a)))))
	(iter a 0))

;;; 1.31a
(define (product term a next b)
 	(if (> a b)
  		1
  		(* (term a)
  			(product term (next a) next b))))

(define (pi/4-approx terms)
	(define (term x)
	 	(if (even? x)
	  		(/ (+ 2.0 x) (+ 1 x))
	  		(/ (+ 1.0 x) (+ 2 x))))
	(product term 1 inc terms))

;;; 1.31b
(define (iterative-product term a next b)
	(define (iter a result)
	 	(if (> a b)
	  		result
     		(iter (next a) (* result (term a)))))
	(iter a 1))

;;; 1.32a
(define (accumulate combiner null-value term a next b)
	(if (> a b)
		null-value
		(combiner (term a) 
			(accumulate combiner null-value term (next a) next b))))

(define (a-sum term a next b)
	(accumulate + 0 term a next b))

(define (a-product term a next b)
  (accumulate * 1 term a next b))

;;; 1.32b
(define (iterative-accumulate combiner null-value term a next b)
	(define (iter a result)
	 	(if (> a b)
	 		result
	 		(iter (next a) (combiner result (term a)))))
	(iter a null-value))

(define (a-iterative-sum term a next b)
	(iterative-accumulate + 0 term a next b))

(define (a-iterative-product term a next b)
	(iterative-accumulate * 1 term a next b))

;;; 1.33
(define (filtered-accumulate predicate combiner null-value term a next b)
	(if (> a b)
  		null-value
  		(combiner 
  			(if (predicate a)
  				(term a)
  				null-value)
  			(filtered-accumulate predicate combiner null-value term (next a) next b))))

;;; 1.33a
(define (sum-of-primes-squared a b)
	(define (square x)
		(* x x))
	(filtered-accumulate prime? + 0 square a inc b))

;;; 1.33b
(define (relatively-prime-product n)
	(define (identity x)
		x)
	(define (relatively-prime? i)
	  (= 1 (gcd i n)))
 	(filtered-accumulate relatively-prime? * 1 identity 1 inc n))


