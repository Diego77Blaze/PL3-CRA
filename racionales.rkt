#lang racket

(require racket/include)
(include "enteros.rkt")


(define reduccanonica
  (lambda (x)
    (lambda (y)
      ((restoent x) y)
     )
   )
)



(define escerorac
  (lambda (x)
    (lambda (y)
      (escerorac ((reduccanonica x) y))
     )
   )
)


(define noescerorac
  (lambda (x)
    (lambda(y)
      (neg ((escerorac x) y))
     )
   )
)

(define sumarac
  (lambda (num1)
    (lambda (num2)
      (lambda (y)
        ((reduccanonica ((sumarac num1) num2)) y)
       )
     )
   )
)

(define restarac
  (lambda (num1)
    (lambda (num2)
      (lambda (y)
        ((reduccanonica ((restarac num1) num2)) y)
       )
     )
   )
)