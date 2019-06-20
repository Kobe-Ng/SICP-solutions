MIT/GNU Scheme running under OS X

Copyright (C) 2014 Massachusetts Institute of Technology
This is free software; see the source for copying conditions. There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Image saved on Friday September 28, 2018 at 5:08:35 PM
  Release 9.2 || Microcode 15.3 || Runtime 15.7 || SF 4.41 || LIAR/C 4.118
  Edwin 3.116

1 ]=> 
;Value: element-of-set?

1 ]=> 
;Value: adjoin-set

1 ]=> 
;Value: intersection-set

1 ]=> 
;Value: union-set

1 ]=> (define set1 (adjoin-set 2 (adjoin-set 3 (adk))))

;Unbound variable: adk
;To continue, call RESTART with an option number:
; (RESTART 3) => Specify a value to use instead of adk.
; (RESTART 2) => Define adk to a given value.
; (RESTART 1) => Return to read-eval-print level 1.

2 error> (RESTART 1)

;Abort!

1 ]=> (define set1 (adjoin-set 2 (adjoin-set 3 (adjoin-set 4 '()))))

;Value: set1

1 ]=> set1

;Value 2: (2 3 4)

1 ]=> (define set2 (adjoin-set 1 (adjoin-set 2 (adjoin-set 5 '()))))

;Value: set2

1 ]=> (intersection-set set1 set2)

;Value 3: (2)

1 ]=> (union-set-set set1 set2)

;Unbound variable: union-set-set
;To continue, call RESTART with an option number:
; (RESTART 3) => Specify a value to use instead of union-set-set.
; (RESTART 2) => Define union-set-set to a given value.
; (RESTART 1) => Return to read-eval-print level 1.

2 error> (RESTART 2)


Define union-set-set as: union-set

;Value: ()

1 ]=> 
;Value: union-set

1 ]=> (union-set set1 set2)

;Value 4: (3)

1 ]=> 
;Value: union-set

1 ]=> (union-set set1 set2)

;Value 5: (3 4 1 2 5)

1 ]=> '(x)

;Value 6: (x)

1 ]=> (define (test x)
  '(x))

;Value: test

1 ]=> (test 5)

;Value 7: (x)

1 ]=> (cons 5 '())

;Value 8: (5)

1 ]=> (append 5 '(2 3 4))

;The object 5, passed as an argument to append, is not a list.
;To continue, call RESTART with an option number:
; (RESTART 1) => Return to read-eval-print level 1.

2 error> (RESTART 1)

;Abort!

1 ]=> (append (list 5) '(2 3 4))

;Value 9: (5 2 3 4)

1 ]=> 
;Value: adjoin-set?

1 ]=> 
;Value: adjoin-set1

1 ]=> (adjoin-set1 2 (adjoin-set1 3 (adjoin-set1 1 '())))

;Value 10: (1 2 3)

1 ]=> (define set (adjoin-set1 2 (adjoin-set1 3 (adjoin-set1 1 '()))))

;Value: set

1 ]=> (adjoin-set1 4 set)

;Value 11: (1 4 2 3)

1 ]=> (define set2 (adjoin-set1 1 (adjoin-set1 2 '())))

;Value: set2

1 ]=> set2

;Value 12: (1 2)

1 ]=> (adjoin-set1 4 set2)

;Value 13: (1 4 2)

1 ]=> 
;Value: adjoin-set1

1 ]=> (adjoin-set1 4 set2)

;Value 14: (4 1 2)

1 ]=> 
;Value: adjoin-set1

1 ]=> (adjoin-set1 4 set2)

;Value 15: (1 4 2)

1 ]=> (append '(1 2) (list 4))

;Value 16: (1 2 4)

1 ]=> (< 4 2)

;Value: #f

1 ]=> (cons 1 (cons 2 (append '() 4)))

;Value 17: (1 2 . 4)

1 ]=> (cons 1 (cons 2 (append '() (list 4))))

;Value 18: (1 2 4)

1 ]=> (car set1)

;Value: 2

1 ]=> (car set2)

;Value: 1

1 ]=> 
;Value: adjoin-set1

1 ]=> 
;Value: adjoin-set

1 ]=> (adjoin-set 4 set1)

;Value 19: (2 3 4)

1 ]=> set1

;Value 2: (2 3 4)

1 ]=> (adjoin-set 3 set1)

;Value 20: (2 3 4)

1 ]=> (adjoin-set 6 set1)

;Value 21: (2 3 4 6)

1 ]=> (adjoin-set 1 set1)

;Value 22: (1 2 3 4)

1 ]=> (adjoin-set 3.5 set1)

;Value 23: (2 3 3.5 4)

1 ]=> 