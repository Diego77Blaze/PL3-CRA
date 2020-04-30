#lang racket

(require racket/include)
(include "enteros.rkt")

(define test_racionales (lambda (r)
                      ((comprobar (primero r)) (comprobar (segundo r)))))

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

(define suma_racionales
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

(define resta_racionales
  (lambda (num1)
    (lambda (num2)
      (reduc_canonica
         ((restaent
           (prodent(primero num1)(mcment(segundo num1)(segundo num2)))
           (prodent(primero num2)(mcment(segundo num1)(segundo num2))))
          )
         (mcment(segundo num1)(segundo num2))
         )
     )
   )
)

(define prod_racionales
  (lambda (num1)
    (lambda (num2)
      (reduc_canonica
       ((prodent (primero num1) (primero num2)))
       ((prodent (segundo num1) (segundo num2)))
       )
      )
    )
  )

(define div_racionales
  (lambda (num1)
    (lambda (num2)
      (reduc_canonica
       ((prodent (primero num1) (primero (inverso_racionales num2))))
       ((prodent (segundo num1) (segundo (inverso_racionales num2))))
       )
      )
    )
  )

(define inverso_racionales
  (lambda (num)
    (reduc_canonica
     (segundo num)
     (primero num)
     )
    )
  )