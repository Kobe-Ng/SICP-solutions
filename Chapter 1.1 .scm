(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve-sqrt guess x)
                 x)))

(define (good-enough? guess x)
  (< 0.99999 (/ (improve-sqrt guess x) guess) 1.00001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve-sqrt guess x)
  (average guess (/ x guess)))

(define (square x)
  (* x x))

(define (sqrt x)
	(sqrt-iter 1.0 x))

(define (improve-cube-root guess x)
	(/ 
	(+ (/ x (square guess))
		(* 2 guess))
	3))
(define (good-enough-cube? guess x)
	(< 0.99999 (/ (improve-cube-root guess x) guess) 1.00001))

(define (cube-root-iter guess x)
	(if(good-enough-cube? guess x)
		guess
		(cube-root-iter (improve-cube-root guess x)
						x)))
(define (cube-root x)
	(cube-root-iter 1.0 x))

(define (cube x)
	(* x x x))
