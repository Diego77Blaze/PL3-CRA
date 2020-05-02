#lang racket

(require racket/include)
(include "enteros.rkt")

;; Test para comprobar operaciones con números racionales
(define test_racionales (lambda (r)
                      (list (testenteros (primero r)) (testenteros (segundo r)))))

;; Verifica si el número es 0
(define escero_racional
  (lambda (x)
    (escero (primero x))
    )
  )

;; Verifica si el número no es 0
(define noescero_racional
  (lambda (x)
    (neg(escero (primero x)))
    )
  )

;; Obtiene la reducciona canónica de un número racional positivo
(define reduc_canonica
  (lambda (x)
    (lambda (y)
      ;; Se obtiene el cociente de la división del dividendo como del divisor de la
      ;; la fracción diviendolos ambos entre el mcd entre ambos
      ((par
        ((cocienteent x)
         ((mcdent x) y))
        )
       ((cocienteent y)
        ((mcdent x) y)))
      )
    )
  )
;; Obtiene la suma de dos números racionales
(define suma_racionales
  (lambda (num1)
    (lambda (num2)
        ((reduc_canonica
          ((sument
            ((prodent
              (primero num1))
             ((cocienteent ;; Obtiene el cociente entre el mcm entre los divisores y el divisor trabajado de la fracción
               ((mcment
                 (segundo num1))
                (segundo num2)))
              (segundo num1))))
           ((prodent
             (primero num2))
            ((cocienteent ;; Obtiene el cociente entre el mcm entre los divisores y el divisor trabajado de la fracción
              ((mcment
                (segundo num1))
               (segundo num2)))
             (segundo num2)))))
         ((mcment(segundo num1))(segundo num2))
         )
      )
    )
  )

;; Obtiene la resta de dos números racionales
(define resta_racionales
  (lambda (num1)
    (lambda (num2)
      ((reduc_canonica
        ((restaent
          ((prodent
            (primero num1))
           ((cocienteent ;; Obtiene el cociente entre el mcm entre los divisores y el divisor trabajado de la fracción
             ((mcment
               (segundo num1))
              (segundo num2)))
            (segundo num1))))
         ((prodent
           (primero num2))
          ((cocienteent ;; Obtiene el cociente entre el mcm entre los divisores y el divisor trabajado de la fracción
            ((mcment
              (segundo num1))
             (segundo num2)))
           (segundo num2)))))
       ((mcment(segundo num1))(segundo num2))
       )
      )
    )
  )

;; Obtiene el producto de dos números racionales
(define prod_racionales
  (lambda (num1)
    (lambda (num2)
      ((reduc_canonica
        ((prodent ;; Producto de los dividendos de los números
          (primero num1))
         (primero num2))
         )
       ((prodent ;; Producto de los divisores de los dos números
         (segundo num1))
        (segundo num2))
         )
      )
    )
  )

;; Obtiene la división de dos números racionales
(define div_racionales
  (lambda (num1)
    (lambda (num2)
      ((reduc_canonica
        ((prodent ;; Producto de el dividendo del primer número y el divisor del segundo
          (primero num1))
         (primero (inverso_racionales num2)))
        )
       ((prodent ;; Producto de el divisor del primer número y el dividendo del segundo
         (segundo num1))
        (segundo (inverso_racionales num2)))
       )
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

;; Verifica si una fracción es mayor que otra
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
;; Verifica si una fracción es menor que otra
(define menor_racional
  (lambda (num1)
    (lambda (num2)
      ((esmenorent
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
;; Verifica si dos números racionales son iguales
(define esigual_racional
  (lambda (num1)
    (lambda (num2)
      (and
       ;; Las reducciones canónicas de los dividendos tienen que ser iguales
       ;; entre sí al igual que lo deben entre sí los divisores
       ((esigualent
         (primero
          ((reduc_canonica
            (primero num1))
           (segundo num1)))
         )
        (primero
         ((reduc_canonica
           (primero num2))
          (segundo num2)))
        )
       ((esigualent
         (segundo
          ((reduc_canonica
            (primero num1))
           (segundo num1)))
        )
        (segundo
         ((reduc_canonica
           (primero num2))
          (segundo num2)))
        )
       )
      )
    )
  )

;; Define la estructura de una matriz 2x2
(define definir_matriz
  (lambda (a)
    (lambda (b)
      (lambda (c)
        (lambda (d)
          ((par ((par a) b)) ((par c) d))))))
  )

;; Test para comprobar operaciones con matrices
(define test_matriz
  (lambda (m)
    (list (list (test_racionales (primero (primero m))) (test_racionales (segundo (primero m))))
          (list (test_racionales (primero (segundo m))) (test_racionales (segundo (segundo m))))
          )
    )
  )

;; Realiza la suma de dos matrices
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

;; Realiza la resta de dos matrices
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

;; Realiza el producto de dos matrices
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

;; Realiza el cuadrado de una matriz
(define cuadrado_matrices
  (lambda (matriz)
    ((prod_matrices matriz) matriz)
    )
  )

;; Realiza el determinante de una matriz
(define determinante
  (lambda (matriz)
    ((resta_racionales
      ((prod_racionales (primero (primero matriz))) (segundo (segundo matriz))))
     ((prod_racionales (segundo (primero matriz))) (primero (segundo matriz)))
     )
    )
  )

;; Relacia la potencia de una matriz
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
                         (((par? n)
                           ;; Exponente par
                           (lambda (no_use1)
                             (cuadrado_matrices (f ((cocientenat n) deux)))
                             )
                           ; Exponente impar
                           (lambda (no_use1)
                             ((prod_matrices matriz) (f (predecesor n)))
                             )
                           ) zero)
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

;; Relacia la potencia de una matriz
(define potencia_matrices
  (lambda (matriz)
    (lambda (num)
      (((escero num)
        (lambda (no_use) matriz)
        (lambda (no_use) ((potecia_matricesaux matriz) num))) zero)
      )
    )
  )

