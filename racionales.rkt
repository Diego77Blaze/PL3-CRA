#lang racket

(require racket/include)
(include "enteros.rkt")

;; Obtiene la reducciona canónica de un número racional
(define reduc_canonica
  (lambda (x)
    (lambda (y)
      ;; Se obtiene el cociente de la división del dividendo como del divisor de la
      ;; la fracción diviendolos ambos entre el mcd entre ambos
      ((par (cocienteent x((mcdent x) y)))
       (cocienteent y((mcdent x) y)))      
     )
   )
)

(define escero_racional
  (lambda (x)
      (escero_racional ((reduc_canonica (primero x)) (segundo x)))
     )
)


(define noescero_racional
  (lambda (x)
      (neg ((escero_racional (primero x) (segundo x))))
  )
)

(define suma_racional
  (lambda (num1)
    (lambda (num2)
        (reduc_canonica
         ((sument
           (prodent(primero num1)(mcment(segundo num1)(segundo num2)))
           (prodent(primero num2)(mcment(segundo num1)(segundo num2))))
          )
         (mcment(segundo num1)(segundo num2))
         )
     )
   )
)

(define resta_racional
  (lambda (num1)
    (lambda (num2)
      (lambda (y)
        ((reduc_canonica ((resta_racional num1) num2)) y)
       )
     )
   )
)