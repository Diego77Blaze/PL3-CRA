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



(define escero_racional
  (lambda (x)
      (escero_racional ((reduccanonica (primero x)) (segundo x)))
     )
)


(define noescero_racional
  (lambda (x)
      (neg ((escero_racional (primero x) (segundo x))))
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