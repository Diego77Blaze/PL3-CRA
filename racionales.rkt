#lang racket

(require racket/include)
(include "enteros.rkt")

;; Test para comprobar operaciones con números racionales
(define test_racionales (lambda (r)
                      (list (testenteros (primero r)) (testenteros (segundo r)))))

;; Verifica si el número es 0
;;(escero_racional ((par cero) uno))
;;#<procedure:true>
(define escero_racional
  (lambda (x)
    (esceroent (primero x))
    )
  )

;; Verifica si el número no es 0
;;(noescero_racional ((par cero) uno))
;;#<procedure:false>
(define noescero_racional
  (lambda (x)
    (neg(escero_racional (primero x)))
    )
  )

;; Devuelve un número racional ajustando el los negativos que pueda tener y ajustar la doble negación como un número positivo
;; y el negativo en el denominador como un número negativo pero que tiene el menos en el numerador
;; (test_racionales (ajustar_negativo_racional ((par uno) -uno)))
;; (-1 1)
;; (test_racionales (ajustar_negativo_racional ((par -uno) -uno)))
;; (1 1)
;; (test_racionales (ajustar_negativo_racional ((par uno) uno)))
;; (1 1)

(define ajustar_negativo_racional
  (lambda (num)
    ((negativo (segundo num)) ;; Denominador negativo
     ((negativo (primero num)) ;; Numerador negativo
      ((par (absoluto (primero num))) (absoluto (segundo num)))
      ((par ((restaent cero) (primero num))) (absoluto (segundo num)))
      )
     num
     )
    )
  )

;; Obtiene la reducciona canónica de un número racional positivo
;;(test_racionales ((reduc_canonica dos)cuatro))
;;(1 2)
(define reduc_canonica
  (lambda (x)
    (lambda (y)
      ;; Se obtiene el cociente de la división del dividendo como del divisor de la
      ;; la fracción diviendolos ambos entre el mcd entre ambos
      (ajustar_negativo_racional 
        ((par
          ((cocienteent x)
           ((mcdent x) y))
          )
         ((cocienteent y)
          ((mcdent x) y)))
        )
      )
    )
  )

;; Obtiene la suma de dos números racionales
;;(test_racionales ((suma_racionales ((par uno) dos)) ((par uno) dos)))
;;(1 1)
(define suma_racionales
  (lambda (num1)
    (lambda (num2)
        ((reduc_canonica
          ((sument
            ((prodent
              (primero (ajustar_negativo_racional num1)))
             ((cocienteent ;; Obtiene el cociente entre el mcm entre los divisores y el divisor trabajado de la fracción
               ((mcment
                 (segundo (ajustar_negativo_racional num1)))
                (segundo (ajustar_negativo_racional num2))))
              (segundo (ajustar_negativo_racional num1)))))
           ((prodent
             (primero (ajustar_negativo_racional num2)))
            ((cocienteent ;; Obtiene el cociente entre el mcm entre los divisores y el divisor trabajado de la fracción
              ((mcment
                (segundo (ajustar_negativo_racional num1)))
               (segundo (ajustar_negativo_racional num2))))
             (segundo (ajustar_negativo_racional num2))))))
         ((mcment(segundo num1))(segundo num2))
         )
      )
    )
  )

;; Obtiene la resta de dos números racionales
;;(test_racionales ((resta_racionales ((par siete) cinco)) ((par cuatro) cinco)))
;;(3 5)
(define resta_racionales
  (lambda (num1)
    (lambda (num2)
      ((reduc_canonica
        ((restaent
          ((prodent
            (primero (ajustar_negativo_racional num1)))
           ((cocienteent ;; Obtiene el cociente entre el mcm entre los divisores y el divisor trabajado de la fracción
             ((mcment
               (segundo (ajustar_negativo_racional num1)))
              (segundo (ajustar_negativo_racional num2))))
            (segundo (ajustar_negativo_racional num1)))))
         ((prodent
           (primero (ajustar_negativo_racional num2)))
          ((cocienteent ;; Obtiene el cociente entre el mcm entre los divisores y el divisor trabajado de la fracción
            ((mcment
              (segundo (ajustar_negativo_racional num1)))
             (segundo (ajustar_negativo_racional num2))))
           (segundo (ajustar_negativo_racional num2))))))
       ((mcment(segundo (ajustar_negativo_racional num1)))(segundo (ajustar_negativo_racional num2)))
       )
      )
    )
  )

;; Obtiene el producto de dos números racionales
;;(test_racionales ((prod_racionales ((par uno) dos)) ((par cuatro) uno)))
;;(2 1)
(define prod_racionales
  (lambda (num1)
    (lambda (num2)
      ((reduc_canonica
        ((prodent ;; Producto de los dividendos de los números
          (primero (ajustar_negativo_racional num1)))
         (primero (ajustar_negativo_racional num2)))
         )
       ((prodent ;; Producto de los divisores de los dos números
         (segundo (ajustar_negativo_racional num1)))
        (segundo (ajustar_negativo_racional num2)))
         )
      )
    )
  )

;; Obtiene la división de dos números racionales
;;(test_racionales ((prod_racionales ((par cuatro) dos)) ((par cuatro) uno)))
;;(8 1)
(define div_racionales
  (lambda (num1)
    (lambda (num2)
      ((reduc_canonica
        ((prodent ;; Producto de el dividendo del primer número y el divisor del segundo
          (primero (ajustar_negativo_racional num1)))
         (primero (inverso_racionales (ajustar_negativo_racional num2))))
        )
       ((prodent ;; Producto de el divisor del primer número y el dividendo del segundo
         (segundo (ajustar_negativo_racional num1)))
        (segundo (inverso_racionales (ajustar_negativo_racional num2))))
       )
      )
    )
  )

;; Obtiene la fracción invertida
;;(test_racionales (inverso_racionales ((par tres) siete)))
;(7 3)
(define inverso_racionales
  (lambda (num)
    (ajustar_negativo_racional
     ((reduc_canonica
       (segundo num))
      (primero num)
      )
     )
    )
  )

;; Verifica si una fracción es mayor que otra
;;((mayor_racional ((par tres) dos)) ((par dos) dos))
;;#<procedure:true>
(define mayor_racional
  (lambda (num1)
    (lambda (num2)
      ((esmayorent
            ((prodent
              (primero (ajustar_negativo_racional num1)))
             ((cocienteent 
              ((mcment
                (segundo (ajustar_negativo_racional num1)))
                (segundo(ajustar_negativo_racional num2))))
              (segundo (ajustar_negativo_racional num1)))))
            ((prodent
              (primero (ajustar_negativo_racional num2)))
             ((cocienteent
              ((mcment
                (segundo (ajustar_negativo_racional num1)))
               (segundo (ajustar_negativo_racional num2))))
              (segundo (ajustar_negativo_racional num2)))))
      )
    )
  )

;; Verifica si una fracción es menor que otra
;;((menor_racional ((par tres) dos)) ((par dos) dos))
;;#<procedure:false>
(define menor_racional
  (lambda (num1)
    (lambda (num2)
      ((esmenorent
            ((prodent
              (primero (ajustar_negativo_racional num1)))
             ((cocienteent 
              ((mcment
                (segundo (ajustar_negativo_racional num1)))
                (segundo (ajustar_negativo_racional num2))))
              (segundo (ajustar_negativo_racional num1)))))
            ((prodent
              (primero (ajustar_negativo_racional num2)))
             ((cocienteent
              ((mcment
                (segundo (ajustar_negativo_racional num1)))
               (segundo (ajustar_negativo_racional num2))))
              (segundo (ajustar_negativo_racional num2)))))
      )
    )
  )
;; Verifica si dos números racionales son iguales
;;((esigual_racional ((par dos) dos)) ((par dos) dos))
;;#<procedure:true>
(define esigual_racional
  (lambda (num1)
    (lambda (num2)
      (and
       ;; Las reducciones canónicas de los dividendos tienen que ser iguales
       ;; entre sí al igual que lo deben entre sí los divisores
       ((esigualent
         (primero
          (ajustar_negativo_racional
           ((reduc_canonica
             (primero num1))
            (segundo num1)))
          )
         )
        (primero
         (ajustar_negativo_racional
          ((reduc_canonica
            (primero num2))
           (segundo num2)))
         )
        )
       ((esigualent
         (segundo
          (ajustar_negativo_racional
           ((reduc_canonica
             (primero num1))
            (segundo num1)))
          )
        )
        (segundo
         (ajustar_negativo_racional
          ((reduc_canonica
            (primero num2))
           (segundo num2)))
         )
        )
       )
      )
    )
  )
;; Verifica si dos números racionales son mayor o iguales entre sí
(define mayorigual_racional
  (lambda(num1)
    (lambda(num2)
      (or
       ((mayor_racional num1)num2)
       ((esigual_racional num1)num2)
       )
      )
   )
 )
;; Verifica si dos números racionales son menor o iguales entre sí
(define menorigual_racional
  (lambda(num1)
    (lambda(num2)
      (or
       ((menor_racional num1)num2)
       ((esigual_racional num1)num2)
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
          ((par ((par (ajustar_negativo_racional a)) (ajustar_negativo_racional b))) ((par (ajustar_negativo_racional c)) (ajustar_negativo_racional d)))))))
  )
(define identidad ((((definir_matriz ((par uno) uno)) ((par cero) uno)) ((par cero) uno)) ((par uno) uno)))
(define matriz_nula ((((definir_matriz ((par cero) uno)) ((par cero) uno)) ((par cero) uno)) ((par cero) uno)))
(define matriz_prueba1 ((((definir_matriz ((par dos) cuatro)) ((par cuatro) cuatro)) ((par -uno) cuatro)) ((par cinco) cuatro)))
(define matriz_prueba2 ((((definir_matriz ((par uno) cuatro))   ((par -cuatro) seis))     ((par dos) ocho)) ((par -dos) tres)))
(define matriz_prueba3 ((((definir_matriz ((par uno) dos))   ((par -cuatro) dos))     ((par dos) dos)) ((par -tres) dos)))

;; Test para comprobar operaciones con matrices
;;(test_matriz identidad)
;;(((1 1) (0 1)) ((0 1) (1 1)))
(define test_matriz
  (lambda (m)
    (list (list (test_racionales (primero (primero m))) (test_racionales (segundo (primero m))))
          (list (test_racionales (primero (segundo m))) (test_racionales (segundo (segundo m))))
          )
    )
  )

;; Realiza la suma de dos matrices 2x2
;;(test_matriz ((suma_matrices matriz_prueba1) matriz_prueba2))
;;(((3 4) (1 3)) ((0 1) (7 12)))
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

;; Realiza la resta de dos matrices 2x2
;;(test_matriz ((resta_matrices matriz_prueba1) matriz_prueba2))
;;(((1 4) (5 3)) ((-1 2) (23 12)))
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

;; Realiza el producto de dos matrices 2x2
;;(test_matriz ((prod_matrices matriz_prueba1) matriz_prueba2))
;;(((3 8) (-1 1)) ((1 4) (-2 3)))
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
;;(test_matriz (cuadrado_matrices matriz_prueba1))
;;(((0 1) (7 4)) ((-7 16) (21 16)))
(define cuadrado_matrices
  (lambda (matriz)
    ((prod_matrices matriz) matriz)
    )
  )

;; Realiza el determinante de una matriz 2x2
;; (test_racionales(determinante identidad))
;; '(1 1)
(define determinante
  (lambda (matriz)
    ((resta_racionales
      ((prod_racionales (primero (primero matriz))) (segundo (segundo matriz))))
     ((prod_racionales (segundo (primero matriz))) (primero (segundo matriz)))
     )
    )
  )

;; Obtiene el rango de una matriz 2x2
;; (testenteros(rango identidad))
;; 2
(define rango
  (lambda (matriz)
    (((escero_racional ((reduc_canonica (primero(determinante matriz))) (segundo(determinante matriz))))
      (lambda (no_use) uno)
      (lambda (no_use) dos)
      )
     true)
    )
  )

;; Verifica si una matriz es invertible o no
;; (inversa? identidad)
;;#<procedure:true>
(define inversa?
  (lambda (matriz)
    (((escero_racional ((reduc_canonica (primero(determinante matriz))) (segundo(determinante matriz))))
      (lambda (no_use) false)
      (lambda (no_use) true)
      )
     true)
    )
  )

;; Obtiene la matriz adjunta de una matriz 2x2
;; (test_matriz (adjunta_matriz (inversa_matriz matriz_prueba1)))
;; '(((5 4) (1 4)) ((-4 4) (2 4)))
(define adjunta_matriz
  (lambda (matriz)
    ((((definir_matriz
         (segundo (segundo matriz)))
       ((negativo (primero(primero(segundo matriz))))
        ;; Número negativo pasa a positivo
        ((par (absoluto (primero(primero(segundo matriz)))))(segundo(primero(segundo matriz))))
        ;; Número positivo pasa a negativo
        ((par ((restaent cero) (primero(primero(segundo matriz))))) (segundo(primero(segundo matriz))))
        ))
      ((negativo (primero(segundo(primero matriz))))
        ;; Número negativo pasa a positivo
       ((par (absoluto (primero(segundo(primero matriz)))))(segundo(segundo(primero matriz))))
       ;;Número positivo pasa a negativo
        ((par ((restaent cero) (primero(segundo(primero matriz))))) (segundo(segundo(primero matriz))))
       ))
     (primero (primero matriz)))
    )
  )

;; Obtiene la matriz inversa de una matriz 
;; (test_matriz (inversa matriz_prueba1))
;; '(((10 7) (2 7)) ((-8 7) (4 7)))
(define inversa
  (lambda (matriz)
    ((inversa? matriz)
     ((producto_coeficiente_matriz (inverso_racionales (determinante matriz))) (adjunta_matriz matriz))
     matriz
     )
    )
  )
;; Aplica un valor a cada una de las posiciones de una matriz 2x2
;; (test_matriz ((producto_coeficiente_matriz ((par dos) dos)) matriz_prueba1))
;; '(((1 2) (1 1)) ((-1 4) (5 4)))
(define producto_coeficiente_matriz
  (lambda (valor)
    (lambda (matriz)
      ((((definir_matriz
           ((prod_racionales valor) (primero(primero matriz))))
         ((prod_racionales valor) (primero(segundo matriz))))
        ((prod_racionales valor) (segundo(primero matriz))))
       ((prod_racionales valor) (segundo(segundo matriz))))
      )
    )
  )

;; Realiza la potencia de una matriz
;;(test_matriz ((potencia_matricesaux matriz_prueba1) deux))
;;(((0 1) (7 4)) ((-7 16) (21 16)))
(define potencia_matricesaux
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

;; Realiza la potencia de una matriz
;;(test_matriz ((potencia_matrices matriz_prueba1) deux)) 
;;(((0 1) (7 4)) ((-7 16) (21 16)))
(define potencia_matrices
  (lambda (matriz)
    (lambda (num)
      (((escero num)
        (lambda (no_use) matriz)
        (lambda (no_use) ((potencia_matricesaux matriz) num))) zero)
      )
    )
  )

