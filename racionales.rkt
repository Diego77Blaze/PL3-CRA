#lang racket

(require racket/include)
(include "enteros.rkt")

;; Test para comprobar operaciones con números racionales
(define test_racionales (lambda (r)
                      (list (comprobar (primero r)) (comprobar (segundo r)))))

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
;;Devuelve true si la primero fraccion es mayor y false si la segunda es mayor
;; funciona con los numeros en español pero solo positivos
#|
(define mayor_racional
  (lambda (num1)
    (lambda (num2)
      ((esmayorent
            ((prodent
              ((par (primero num1)) zero))
             ((cocienteent 
              ((mcment
                ((par (segundo num1)) zero))
               ((par (segundo num2)) zero)))
              ((par (segundo num1)) zero))))
            ((prodent
              ((par (primero num2)) zero))
             ((cocienteent
              ((mcment
                ((par (segundo num1)) zero))
               ((par(segundo num2)) zero)))
              ((par (segundo num2)) zero))))
      )
    )
  )
esta funcion del comentario funciona con los numeros en frances
|#

(define mayor_racional
  (lambda (num1)
    (lambda (num2)
      ((esmayorent
            ((prodent
              ((par (primero(reducir(primero num1)))) zero))
             ((cocienteent 
              ((mcment
                ((par (primero(reducir(segundo num1)))) zero))
               ((par (primero(reducir(segundo num2)))) zero)))
              ((par (primero(reducir(segundo num1)))) zero))))
            ((prodent
              ((par (primero(reducir(primero num2)))) zero))
             ((cocienteent
              ((mcment
                ((par (primero(reducir(segundo num1)))) zero))
               ((par (primero(reducir(segundo num2)))) zero)))
              ((par (primero(reducir(segundo num2)))) zero))))
      )
    )
  )
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

(define definir_matriz
  (lambda (a)
    (lambda (b)
      (lambda (c)
        (lambda (d)
          ((par ((par a) b)) ((par c) d))))))
  )


(define test_matriz
  (lambda (m)
    (list (list (test_racionales (primero (primero m))) (test_racionales (segundo (primero m))))
          (list (test_racionales (primero (segundo m))) (test_racionales (segundo (segundo m))))
          )
    )
  )

(define suma_matrices
  (lambda (matriz1)
    (lambda (matriz2)
      ((((definir_matriz
           ((suma_racionales (primero (primero matriz1))) (primero (primero matriz2))))
         ((suma_racionales (segundo (primero matriz1))) (segundo (primero matriz2))))
        ((suma_racionales (primero (segundo matriz1))) (primero (segundo matriz2))))
       ((suma_racionales (segundo (segundo matriz1))) (segundo (segundo matriz2))))
      )  
    )
  )

(define resta_matrices
  (lambda (matriz1)
    (lambda (matriz2)
      ((((definir_matriz
           ((resta_racionales (primero (primero matriz1))) (primero (primero matriz2))))
         ((resta_racionales (segundo (primero matriz1))) (segundo (primero matriz2))))
        ((resta_racionales (primero (segundo matriz1))) (primero (segundo matriz2))))
       ((resta_racionales (segundo (segundo matriz1))) (segundo (segundo matriz2))))
      )  
    )
  )

(define prod_matrices
  (lambda (matriz1)
    (lambda (matriz2)
      ((((definir_matriz
           ((suma_racionales
             ((prod_racionales (primero (primero matriz1))) (primero (primero matriz2))))
             ((prod_racionales (segundo (primero matriz1))) (primero (segundo matriz2)))))
           ((suma_racionales
             ((prod_racionales (primero (primero matriz1))) (segundo (primero matriz2))))
             ((prod_racionales (segundo (primero matriz1))) (segundo (segundo matriz2)))))
           ((suma_racionales
             ((prod_racionales (primero (segundo matriz1))) (primero (primero matriz2))))
             ((prod_racionales (segundo (segundo matriz1))) (primero (segundo matriz2)))))
           ((suma_racionales
             ((prod_racionales (primero (segundo matriz1))) (segundo (primero matriz2))))
             ((prod_racionales (segundo (segundo matriz1))) (segundo (segundo matriz2)))))
      )
    )
  )

(define determinante
  (lambda (matriz)
    ((resta_racionales
      ((prod_racionales (primero (primero matriz))) (segundo (segundo matriz))))
     ((prod_racionales (segundo (primero matriz))) (primero (segundo matriz)))
     )
    )
  )

(define potecia_matricesaux
    (lambda (matriz)
        (lambda (num)
            ((Y (lambda (f)
                   (lambda (n)
                     ((((esigualnat n) un)
                       (lambda (no_use)
                         matriz
                         )
                       (lambda (no_use)
                         ((prod_matrices matriz) (f (predecesor n)))
                         )
                       )
                      zero)    ; Pasa zero como argumento de no_use
                     )
                    )
                  )
             num)
        )
      )
  )

(define potencia_matrices
  (lambda (matriz)
    (lambda (num)
      (((escero num)
        (lambda (no_use) matriz)
        (lambda (no_use) ((potecia_matricesaux matriz) num))) zero)
      )
    )
  )

