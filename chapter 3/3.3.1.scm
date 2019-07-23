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
