;;; 3.12

;;; (cdr x)
;;; (b)
;;; (cdr x)
;;; (b c d)
;;; The append! function changes x so that it's cdr
;;; includes y

;;; 3.13

;;; The interpreter goes into an infinite loop

;;; 3.14

;;; It seems like the procedure reverses x
;;; The original list is changed into the
;;; first element of the list

;;; 3.15

;;; The essence is that set-wow on z1
;;; changes a to wow, but the second pair
;;; points to that same a leading to the results
;;; ((wow b) wow b). For z2, the two elements a
;;; in the list are distinct, so the result is 
;;; ((wow b) a b). That second a is not changed.

;;; 3.16
;;; '(a b c) provides a value of 3
;;; '((a b) c) provides a value of 4
;;; '(((a) b) (c) d) provides a value of 7
;;; ignoring the box structure shennanigans,
;;; this procedure is quite awful

;;; 3.17

(define (count-pairs x) 
   (let ((encountered '())) 
     (define (iter x) 
       (if (or (not (pair? x)) (memq x encountered)) 
         0 
         (begin 
           (set! encountered (cons x encountered)) 
           (+ (iter (car x)) 
              (iter (cdr x)) 
              1)))) 
   (helper x))) 

;;; 3.18

 (define (cycle? x) 
   (define visited nil) 
   (define (iter x) 
     (set! visited (cons x visited)) 
     (cond ((null? (cdr x)) false) 
           ((memq (cdr x) visited) true) 
           (else (iter (cdr x))))) 
   (iter x)) 

