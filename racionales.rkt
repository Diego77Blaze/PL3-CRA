#lang racket

(require racket/include)
(include "enteros.rkt")

;; Test para comprobar operaciones con números racionales
(define test_racionales (lambda (r)
                      (cons (comprobar (primero r)) (comprobar (segundo r)))))

;; Verifica si el número es 0
(define escero_racional
  (lambda (x)
    (escero (primero x))
    )
)

;; Verifica si el número no es 0
(define noescero_racional
  (lambda (x)
    (escero (primero x))
    )
)

;; Obtiene la reducciona canónica de un número racional positivo
(define reduc_canonica
  (lambda (x)
    (lambda (y)
      ;; Se obtiene el cociente de la división del dividendo como del divisor de la
      ;; la fracción diviendolos ambos entre el mcd entre ambos
      ((par
        (primero
          ((cocienteent ((par x) zero))
           ((mcdent ((par x) zero)) ((par y) zero)))
          )
         )
       (primero
        ((cocienteent ((par y) zero))
         ((mcdent ((par x) zero)) ((par y) zero)))))      
     )
   )
)
;; Obtiene la suma de dos números racionales
(define suma_racionales
  (lambda (num1)
    (lambda (num2)
        ((reduc_canonica
         (primero
          ((sument
            ((prodent
              ((par (primero num1)) zero))
             ((cocienteent ;; Obtiene el cociente entre el mcm entre los divisores y el divisor trabajado de la fracción
              ((mcment
                ((par (segundo num1)) zero))
               ((par (segundo num2)) zero)))
              ((par (segundo num1)) zero))))
            ((prodent
              ((par (primero num2)) zero))
             ((cocienteent ;; Obtiene el cociente entre el mcm entre los divisores y el divisor trabajado de la fracción
              ((mcment
                ((par (segundo num1)) zero))
               ((par (segundo num2)) zero)))
              ((par (segundo num2)) zero))))))
         (primero
          ((mcment((par (segundo num1)) zero))((par (segundo num2)) zero)))
         )
     )
   )
)

;; Obtiene la resta de dos números racionales
(define resta_racionales
  (lambda (num1)
    (lambda (num2)
      ((reduc_canonica
         (primero
          ((restaent
            ((prodent
              ((par (primero num1)) zero))
             ((cocienteent ;; Obtiene el cociente entre el mcm entre los divisores y el divisor trabajado de la fracción
              ((mcment
                ((par (segundo num1)) zero))
               ((par (segundo num2)) zero)))
              ((par (segundo num1)) zero))))
            ((prodent
              ((par (primero num2)) zero))
             ((cocienteent ;; Obtiene el cociente entre el mcm entre los divisores y el divisor trabajado de la fracción
              ((mcment
                ((par (segundo num1)) zero))
               ((par (segundo num2)) zero)))
              ((par (segundo num2)) zero))))))
         (primero
          ((mcment((par (segundo num1)) zero))((par (segundo num2)) zero)))
         )
     )
   )
)

;; Obtiene el producto de dos números racionales
(define prod_racionales
  (lambda (num1)
    (lambda (num2)
      ((reduc_canonica
        (primero
         ((prodent ;; Producto de los dividendos de los números
           ((par (primero num1)) zero))
          ((par (primero num2)) zero))
         ))
       (primero
         ((prodent ;; Producto de los divisores de los dos números
           ((par (segundo num1)) zero))
          ((par (segundo num2)) zero))
         ))
        )
    )
  )

;; Obtiene la división de dos números racionales
(define div_racionales
  (lambda (num1)
    (lambda (num2)
      ((reduc_canonica
        (primero
         ((prodent ;; Producto de el dividendo del primer número y el divisor del segundo
           ((par (primero num1)) zero))
          ((par (primero (inverso_racionales num2))) zero))
         ))
       (primero
         ((prodent ;; Producto de el divisor del primer número y el dividendo del segundo
           ((par (segundo num1)) zero))
          ((par (segundo (inverso_racionales num2))) zero))
         ))
        )
    )
  )

;; Obtiene la fracción invertida
(define inverso_racionales
  (lambda (num)
    ((reduc_canonica
     (segundo num))
     (primero num)
     )
    )
  )

;;(define equal_num
;;  (lambda (num1)
;;    (lambda (num2)
      
;;      )
;;    )
;;  )

;; Verifica si dos números racionales son iguales
(define esigual_racional
  (lambda (num1)
    (lambda (num2)
      (and
       ;; Las reducciones canónicas de los dividendos tienen que ser iguales
       ;; entre sí al igual que lo deben entre sí los divisores
       ((esigualent
         ((par
          (primero
           ((reduc_canonica
             (primero num1))
            (segundo num1))))
          zero))
         ((par
           (primero
            ((reduc_canonica
              (primero num2))
             (segundo num2))))
          zero))
       ((esigualent
         ((par
           (segundo
            ((reduc_canonica
              (primero num1))
             (segundo num1))))
          zero))
        ((par
          (segundo
           ((reduc_canonica
             (primero num2))
            (segundo num2))))
         zero)))
      )
    )
  )