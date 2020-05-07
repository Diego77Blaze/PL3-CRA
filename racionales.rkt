(load "enteros.rkt")

;; Test para comprobar operaciones con números racionales
;; (1 2)
(define test_racionales (lambda (r)
                      (list (testenteros (primero r)) (testenteros (segundo r)))))

;; Verifica si el número es 0
;; (escero_racional ((par cero) uno))
;; #<procedure:true>
(define escero_racional
  (lambda (x)
    ;; Comprueba si el numerador del número racional es 0
    (esceroent (primero x))
    )
  )

;; Verifica si el número no es 0
;; (noescero_racional ((par cero) uno))
;; #<procedure:false>
(define noescero_racional
  (lambda (x)
    ;; Obtiene el resultado opuesto de escero_racional
    (neg(escero_racional (primero x)))
    )
  )

;; Devuelve un número racional ajustando el que se considere positivo o negativo el número
;; de acuerdo a que tanto denominador y numerador sean negativos y por tanto, se pasa a
;; tener un número positivo, o bien, si el denominador es negativo, pero el numerador no
;; el numerador pasa a ser negativo y el denominador a positivo para permitir una mayor
;; facilidad y homogeneidad a los cálculos entre los racionales
;; (test_racionales (ajustar_negativo_racional ((par uno) -uno)))
;; (-1 1)
;; (test_racionales (ajustar_negativo_racional ((par -uno) -uno)))
;; (1 1)
;; (test_racionales (ajustar_negativo_racional ((par uno) uno)))
;; (1 1)

(define ajustar_negativo_racional
  (lambda (num)
    ((negativo (segundo num)) ;; El denominador del número racional es negativo
     ((negativo (primero num)) ;; El numerador del número racional es negativo
      ((par (absoluto (primero num))) (absoluto (segundo num))) ;; Se convierte el número racional a positivo
      ((par ((restaent cero) (primero num))) (absoluto (segundo num))) ;; El signo menos pasa del denominador al numerador
      )
     num ;; No se han producido ningún cambio (número positivo o número negativo pero con numerador negativo y denominador positivo)
     )
    )
  )

;; Obtiene la reducciona canónica de un número racional 
;; (test_racionales ((reduc_canonica dos)cuatro))
;; (1 2)
(define reduc_canonica
  (lambda (x)
    (lambda (y)
      ;; Se obtiene el cociente tanto del numerador como del denominador
      ;; de un número racional con el mcd entre el mcd entre ambos
      (ajustar_negativo_racional 
        ((par
          ((cocienteent x) ;; Cociente entre el numerador y el mcd del numerador y el denominador
           ((mcdent x) y))
          )
         ((cocienteent y) ;; Cociente entre el denominador y el mcd del numerador y el denominador
          ((mcdent x) y)))
        )
      )
    )
  )

;; Obtiene la suma de dos números racionales
;; (test_racionales ((suma_racionales ((par uno) dos)) ((par uno) dos)))
;; (1 1)
(define suma_racionales
  (lambda (num1)
    (lambda (num2)
        ((reduc_canonica ;; El resultado se deja representado en su forma canónica
          ((sument ;; Suma de los numeradores de dos números racionales ajustados al mcm de sus denominadores iniciales
            ((prodent
              ;; Producto de numerador del primer número racional con el cociente
              ;; entre el mcm entre los denominadores de los dos números racionales y el denominador del primero
              (primero (ajustar_negativo_racional num1))) ;; Numerador del primer número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
             ((cocienteent ;; Obtiene el cociente entre el mcm entre los denominadores de los dos números racionales y el denominador del primero
               ((mcment
                 (segundo (ajustar_negativo_racional num1))) ;; Denominador del primer número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
                (segundo (ajustar_negativo_racional num2)))) ;; Denominador del segundo número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
              (segundo (ajustar_negativo_racional num1))))) ;; Denominador del primer número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
           ((prodent
             ;; Producto de numerador del segundo número racional con el cociente
             ;; entre el mcm entre los denominadores de los dos números racionales y el denominador del segundo
             (primero (ajustar_negativo_racional num2)))
            ((cocienteent ;; Obtiene el cociente entre el mcm entre los denominadores de los dos números racionales y el denominador del primero
              ((mcment
                (segundo (ajustar_negativo_racional num1))) ;; Denominador del primer número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
               (segundo (ajustar_negativo_racional num2)))) ;; Denominador del segundo número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
             (segundo (ajustar_negativo_racional num2)))))) ;; Denominador del segundo número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
         ((mcment(segundo num1))(segundo num2)) ;; mcm entre los denominadores de ambos números racionales
         )
      )
    )
  )

;; Obtiene la resta de dos números racionales
;; (test_racionales ((resta_racionales ((par siete) cinco)) ((par cuatro) cinco)))
;; (3 5)
(define resta_racionales
  (lambda (num1)
    (lambda (num2)
      ((reduc_canonica ;; El resultado se deja representado en su forma canónica
          ((restaent ;; Resta de los numeradores de dos números racionales ajustados al mcm de sus denominadores iniciales
            ((prodent
              ;; Producto de numerador del primer número racional con el cociente
              ;; entre el mcm entre los denominadores de los dos números racionales y el denominador del primero
              (primero (ajustar_negativo_racional num1))) ;; Numerador del primer número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
             ((cocienteent ;; Obtiene el cociente entre el mcm entre los denominadores de los dos números racionales y el denominador del primero
               ((mcment
                 (segundo (ajustar_negativo_racional num1))) ;; Denominador del primer número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
                (segundo (ajustar_negativo_racional num2)))) ;; Denominador del segundo número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
              (segundo (ajustar_negativo_racional num1))))) ;; Denominador del primer número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
           ((prodent
             ;; Producto de numerador del segundo número racional con el cociente
             ;; entre el mcm entre los denominadores de los dos números racionales y el denominador del segundo
             (primero (ajustar_negativo_racional num2)))
            ((cocienteent ;; Obtiene el cociente entre el mcm entre los denominadores de los dos números racionales y el denominador del primero
              ((mcment
                (segundo (ajustar_negativo_racional num1))) ;; Denominador del primer número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
               (segundo (ajustar_negativo_racional num2)))) ;; Denominador del segundo número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
             (segundo (ajustar_negativo_racional num2)))))) ;; Denominador del segundo número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
         ((mcment(segundo num1))(segundo num2)) ;; mcm entre los denominadores de ambos números racionales
         )
      )
    )
  )

;; Obtiene el producto de dos números racionales
;; (test_racionales ((prod_racionales ((par uno) dos)) ((par cuatro) uno)))
;; (2 1)
(define prod_racionales
  (lambda (num1)
    (lambda (num2)
      ((reduc_canonica ;; El resultado se deja representado en su forma canónica
        ((prodent ;; Producto de los numeradores de los números
          (primero (ajustar_negativo_racional num1))) ;; Numerador del primer número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
         (primero (ajustar_negativo_racional num2))) ;; Numerador del segundo número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
         )
       ((prodent ;; Producto de los denominadores de los dos números
         (segundo (ajustar_negativo_racional num1))) ;; Denominador del primer número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
        (segundo (ajustar_negativo_racional num2))) ;; Numerador del segundo número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
         )
      )
    )
  )

;; Obtiene la división de dos números racionales
;; (test_racionales ((div_racionales ((par cuatro) dos)) ((par cuatro) uno)))
;; (1 2)
(define div_racionales
  (lambda (num1)
    (lambda (num2)
      ((reduc_canonica ;; El resultado se deja representado en su forma canónica
        ((prodent ;; Producto del numerador del primer número y del denominador del segundo número
          (primero (ajustar_negativo_racional num1))) ;; Numerador del primer número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
         (primero (inverso_racionales (ajustar_negativo_racional num2)))) ;; Denominador del segundo número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
        )
       ((prodent ;; Producto del denominador del primer número y del numerador del segundo número
         (segundo (ajustar_negativo_racional num1))) ;; Denominador del primer número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
        (segundo (inverso_racionales (ajustar_negativo_racional num2)))) ;; Numerador del segundo número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
       )
      )
    )
  )

;; Obtiene la fracción invertida
;; (test_racionales (inverso_racionales ((par tres) siete)))
;; (7 3)
(define inverso_racionales
  (lambda (num)
    (ajustar_negativo_racional ;; Se tiene  en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador para ajustar el resultado si es necesario
     ((reduc_canonica ;; El resultado se deja representado en su forma canónica
       (segundo num)) ;; El denominador del número pasa a ser el numerador del número racional inverso
      (primero num) ;; El numerador del número pasa a ser el denominador del número racional inverso
      )
     )
    )
  )

;; Verifica si una fracción es mayor que otra
;; ((mayor_racional ((par tres) dos)) ((par dos) dos))
;; #<procedure:true>
(define mayor_racional
  (lambda (num1)
    (lambda (num2)
      ((esmayorent
        ;; Obtiene los números racionales equivalentes a cada uno de los números proporcionados
        ;; de acuerdo a mcm de sus denominadores y después comprueba si el primero es mayor que el segundo
            ((prodent
              (primero (ajustar_negativo_racional num1))) ;; Numerador del primer número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
             ((cocienteent 
              ((mcment
                (segundo (ajustar_negativo_racional num1))) ;; Denominador del primer número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
                (segundo(ajustar_negativo_racional num2)))) ;; Denominador del segundo número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
              (segundo (ajustar_negativo_racional num1))))) ;; Denominador del primer número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
            ((prodent
              (primero (ajustar_negativo_racional num2))) ;; Numerador del segundo número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
             ((cocienteent
              ((mcment
                (segundo (ajustar_negativo_racional num1))) ;; Denominador del primer número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
               (segundo (ajustar_negativo_racional num2)))) ;; Denominador del segundo número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
              (segundo (ajustar_negativo_racional num2))))) ;; Denominador del segundo número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
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
        ;; Obtiene los números racionales equivalentes a cada uno de los números proporcionados
        ;; de acuerdo a mcm de sus denominadores y después comprueba si el primero es menor que el segundo
        ((prodent
          (primero (ajustar_negativo_racional num1))) ;; Numerador del primer número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
         ((cocienteent 
           ((mcment
             (segundo (ajustar_negativo_racional num1))) ;; Denominador del primer número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
            (segundo(ajustar_negativo_racional num2)))) ;; Denominador del segundo número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
          (segundo (ajustar_negativo_racional num1))))) ;; Denominador del primer número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
       ((prodent
         (primero (ajustar_negativo_racional num2))) ;; Numerador del segundo número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
        ((cocienteent
          ((mcment
            (segundo (ajustar_negativo_racional num1))) ;; Denominador del primer número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
           (segundo (ajustar_negativo_racional num2)))) ;; Denominador del segundo número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
         (segundo (ajustar_negativo_racional num2))))) ;; Denominador del segundo número racional teniendo en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador
      )
    )
  )
;; Verifica si dos números racionales son iguales
;; ((esigual_racional ((par dos) dos)) ((par dos) dos))
;; #<procedure:true>
(define esigual_racional
  (lambda (num1)
    (lambda (num2)
      (and
       ;; Las reducciones canónicas de los numerados tienen que ser iguales
       ;; entre sí al igual que lo deben ser entre sí los denominadores
       ((esigualent ;; Numeradores son iguales
         (primero ;; Numerador del primer número
          (ajustar_negativo_racional ;; Se tiene  en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador para ajustar el resultado si es necesario
           ((reduc_canonica ;; El número se deja representado en su forma canónica
             (primero num1))
            (segundo num1)))
          )
         )
        (primero ;; Numerador del segundo número
         (ajustar_negativo_racional ;; Se tiene  en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador para ajustar el resultado si es necesario
          ((reduc_canonica ;; El número se deja representado en su forma canónica
            (primero num2))
           (segundo num2)))
         )
        )
       ((esigualent ;; Denominadores son iguales
         (segundo ;; Denominador del primer número
          (ajustar_negativo_racional ;; Se tiene  en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador para ajustar el resultado si es necesario
           ((reduc_canonica ;; El número se deja representado en su forma canónica
             (primero num1))
            (segundo num1)))
          )
        )
        (segundo ;; Denominador del segundo número
         (ajustar_negativo_racional ;; Se tiene  en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador para ajustar el resultado si es necesario
          ((reduc_canonica ;; El número se deja representado en su forma canónica
            (primero num2))
           (segundo num2)))
         )
        )
       )
      )
    )
  )
;; Verifica si dos números racionales son mayor o iguales entre sí
;; ((mayorigual_racional ((par tres) cuatro)) ((par tres) cinco))
;; #<procedure:true>
;; ((mayorigual_racional ((par tres) cinco)) ((par tres) cinco))
;; #<procedure:true>
;; ((mayorigual_racional ((par tres) cinco)) ((par tres) cuatro))
;; #<procedure:false>
(define mayorigual_racional
  (lambda(num1)
    (lambda(num2)
      (or
       ((mayor_racional num1)num2) ;; Comprueba si el primer número es mayor que el segundo
       ((esigual_racional num1)num2) ;; Comprueba si son iguales los dos números
       )
      )
   )
 )
;; Verifica si dos números racionales son menor o iguales entre sí
;; ((menorigual_racional ((par tres) cinco)) ((par tres) cinco))
;; #<procedure:true>
;; ((menorigual_racional ((par tres) cuatro)) ((par tres) cinco))
;; #<procedure:false>
;; ((menorigual_racional ((par tres) cinco)) ((par tres) cuatro))
;; #<procedure:true>
(define menorigual_racional
  (lambda(num1)
    (lambda(num2)
      (or
       ((menor_racional num1)num2) ;; Comprueba si el primer número es menor que el segundo
       ((esigual_racional num1)num2) ;; Comprueba si son iguales los dos números
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
          ;; Se obtiene la matriz 2x2 con todos sus números teniendo en cuenta si tiene algún valor negativo, ya sea en el numerador o en el denominador, para ajustar el resultado si es necesario
          ((par ((par (ajustar_negativo_racional a)) (ajustar_negativo_racional b))) ((par (ajustar_negativo_racional c)) (ajustar_negativo_racional d)))))))
  )

;;Definición de matrices prueba
(define identidad ((((definir_matriz ((par uno) uno)) ((par cero) uno)) ((par cero) uno)) ((par uno) uno)))
(define matriz_nula ((((definir_matriz ((par cero) uno)) ((par cero) uno)) ((par cero) uno)) ((par cero) uno)))
(define matriz_prueba1 ((((definir_matriz ((par dos) cuatro)) ((par cuatro) cuatro)) ((par -uno) cuatro)) ((par cinco) cuatro)))
(define matriz_prueba2 ((((definir_matriz ((par uno) cuatro))   ((par -cuatro) seis))     ((par dos) ocho)) ((par -dos) tres)))
(define matriz_prueba3 ((((definir_matriz ((par uno) dos))   ((par -cuatro) dos))     ((par dos) dos)) ((par -tres) dos)))

;; Test para comprobar operaciones con matrices
;; (test_matriz identidad)
;; (((1 1) (0 1)) ((0 1) (1 1)))
(define test_matriz
  (lambda (m)
    (list (list (test_racionales (primero (primero m))) (test_racionales (segundo (primero m))))
          (list (test_racionales (primero (segundo m))) (test_racionales (segundo (segundo m))))
          )
    )
  )

;; Realiza la suma de dos matrices 2x2
;; (test_matriz ((suma_matrices matriz_prueba1) matriz_prueba2))
;; (((3 4) (1 3)) ((0 1) (7 12)))
(define suma_matrices
  (lambda (matriz1)
    (lambda (matriz2)
      ((((definir_matriz
           ((suma_racionales (primero (primero matriz1))) (primero (primero matriz2)))) ;; Suma de los números en la primera fila y primera columna de las dos matrices
         ((suma_racionales (segundo (primero matriz1))) (segundo (primero matriz2)))) ;; Suma de los números en la primera fila y segunda columna de las dos matrices
        ((suma_racionales (primero (segundo matriz1))) (primero (segundo matriz2)))) ;; Suma de los números en la segunda fila y primera columna de las dos matrices
       ((suma_racionales (segundo (segundo matriz1))) (segundo (segundo matriz2)))) ;; Suma de los números en la segunda fila y segunda columna de las dos matrices
      )  
    )
  )

;; Realiza la resta de dos matrices 2x2
;; (test_matriz ((resta_matrices matriz_prueba1) matriz_prueba2))
;; (((1 4) (5 3)) ((-1 2) (23 12)))
(define resta_matrices
  (lambda (matriz1)
    (lambda (matriz2)
      ((((definir_matriz
           ((resta_racionales (primero (primero matriz1))) (primero (primero matriz2)))) ;; Resta de los números en la primera fila y primera columna de las dos matrices
         ((resta_racionales (segundo (primero matriz1))) (segundo (primero matriz2)))) ;; Resta de los números en la primera fila y segunda columna de las dos matrices
        ((resta_racionales (primero (segundo matriz1))) (primero (segundo matriz2)))) ;; Resta de los números en la segunda fila y primera columna de las dos matrices
       ((resta_racionales (segundo (segundo matriz1))) (segundo (segundo matriz2)))) ;; Resta de los números en la segunda fila y segunda columna de las dos matrices
      )  
    )
  )

;; Realiza el producto de dos matrices 2x2
;; (test_matriz ((prod_matrices matriz_prueba1) matriz_prueba2))
;; (((3 8) (-1 1)) ((1 4) (-2 3)))
(define prod_matrices
  (lambda (matriz1)
    (lambda (matriz2)
      ((((definir_matriz
           ((suma_racionales ;; Suma de lo obtenido para la posición de la primera fila y primera columna de la matriz resultante
             ;; Producto de la posición de la primera fila y primera columna de la primera matriz con la posición de la primera fila y primera columna de la segunda matriz
             ((prod_racionales (primero (primero matriz1))) (primero (primero matriz2))))
            ;; Producto de la posición de la primera fila y segunda columna de la primera matriz con la posición de la segunda fila y primera columna de la segunda matriz
             ((prod_racionales (segundo (primero matriz1))) (primero (segundo matriz2)))))
           ((suma_racionales ;; Suma de lo obtenido para la posición de la primera fila y segunda columna de la matriz resultante
             ;; Producto de la posición de la primera fila y primera columna de la primera matriz con la posición de la primera fila y segunda columna de la segunda matriz
             ((prod_racionales (primero (primero matriz1))) (segundo (primero matriz2))))
            ;; Producto de la posición de la primera fila y segunda columna de la primera matriz con la posición de la segunda fila y segunda columna de la segunda matriz
             ((prod_racionales (segundo (primero matriz1))) (segundo (segundo matriz2)))))
           ((suma_racionales ;; Suma de lo obtenido para la posición de la segunda fila y primera columna de la matriz resultante
             ;; Producto de la posición de la segunda fila y primera columna de la primera matriz con la posición de la primera fila y primera columna de la segunda matriz
             ((prod_racionales (primero (segundo matriz1))) (primero (primero matriz2))))
            ;; Producto de la posición de la segunda fila y segunda columna de la primera matriz con la posición de la segunda fila y primera columna de la segunda matriz
             ((prod_racionales (segundo (segundo matriz1))) (primero (segundo matriz2)))))
           ((suma_racionales ;; Suma de lo obtenido para la posición de la segunda fila y segunda columna de la matriz resultante
             ;; Producto de la posición de la segunda fila y primera columna de la primera matriz con la posición de la primera fila y segunda columna de la segunda matriz
             ((prod_racionales (primero (segundo matriz1))) (segundo (primero matriz2))))
            ;; Producto de la posición de la segunda fila y segunda columna de la primera matriz con la posición de la segunda fila y segunda columna de la segunda matriz
             ((prod_racionales (segundo (segundo matriz1))) (segundo (segundo matriz2)))))
      )
    )
  )


;; Realiza el cuadrado de una matriz
;; (test_matriz (cuadrado_matrices matriz_prueba1))
;; (((0 1) (7 4)) ((-7 16) (21 16)))
(define cuadrado_matrices
  (lambda (matriz)
    ;; Realiza el producto de una matriz por si misma
    ((prod_matrices matriz) matriz)
    )
  )

;; Realiza el determinante de una matriz 2x2
;; (test_racionales(determinante identidad))
;; (1 1)
(define determinante
  (lambda (matriz)
    ((resta_racionales ;; Resta entre los resultados obtenidos de la diagonal principal y de la diagonal secundaria 
      ;; Producto de las casillas en la diagonal principal de la matriz
      ;; Producto de la posición de la primera fila y primera columna con la posición de la segunda fila y segunda columna 
      ((prod_racionales (primero (primero matriz))) (segundo (segundo matriz))))
      ;; Producto de las casillas en la diagonal secundaria de la matriz
      ;; Producto de la posición de la primera fila y segunda columna con la posición de la segunda fila y primera columna 
     ((prod_racionales (segundo (primero matriz))) (primero (segundo matriz)))
     )
    )
  )

;; Obtiene el rango de una matriz 2x2
;; (testenteros(rango identidad))
;; 2
(define rango
  (lambda (matriz)
    ;; Comprueba que todos los valores son iguales a cero
    (((and (escero_racional ((reduc_canonica (primero(primero(primero matriz)))) (segundo(primero(primero matriz)))))
          (and (escero_racional ((reduc_canonica (primero(segundo(primero matriz)))) (segundo(segundo(primero matriz)))))
           (and(escero_racional ((reduc_canonica (primero(primero(segundo matriz)))) (segundo(primero(segundo matriz)))))
               (escero_racional ((reduc_canonica (primero(segundo(segundo matriz)))) (segundo(segundo(segundo matriz))))))))
     (lambda (no_use) cero) ;; Matriz nula y por tanto, el rango es 0
     (lambda (no_use)
       (((escero_racional ((reduc_canonica (primero(determinante matriz))) (segundo(determinante matriz)))) ;; Comprueba que el determinante de la matriz es 0
         (lambda (no_use2) uno) ;; Matriz en la que una de las ecuaciones representadas en sus filas, es dependiente de la otra
         (lambda (no_use2) dos) ;; Matriz en la que las ecuaciones representadas en sus filas, son independientes entre sí
         )
        true)
       )
     )true)
    )
  )

;; Verifica si una matriz es invertible o no
;; (inversa? identidad)
;; #<procedure:true>
(define inversa?
  (lambda (matriz)
    ;;Comprueba si el determinante de la matriz es nulo
    (((escero_racional ((reduc_canonica (primero(determinante matriz))) (segundo(determinante matriz))))
      (lambda (no_use) false) ;; Determinante nulo y por ello, no se puede invertir
      (lambda (no_use) true) ;; Determinante no nulo y por ello, se puede invertir
      )
     true)
    )
  )

;; Obtiene la matriz adjunta de una matriz 2x2
;; (test_matriz (adjunta_matriz (inversa matriz_prueba1)))
;; (((5 4) (1 4)) ((-4 4) (2 4)))
(define adjunta_matriz
  (lambda (matriz)
    ((((definir_matriz
         ;; Se intercambian los valores de las posiciones que se encuentran en la diagonal principal entre sí
         ;; y se intercambian los valores y el signo de los mismos de las posiciones que se encuentran en la diagonal secundaria entre sí
         ;; El valor de la posición de la segunda fila y segunda columna, pasa a ser el valor de la posición de la primera fila y primera columna
         ;; Se tiene  en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador para ajustar el resultado si es necesario
         (ajustar_negativo_racional(segundo (segundo matriz))))
       ;; El valor de la posición de la segunda fila y primera columna, pasa a ser el valor de la posición de la primera fila y segunda columna
       ((negativo (primero(ajustar_negativo_racional(primero(segundo matriz))))) ;; Se comprueba que el valor de la posición en la segunda fila y primera columna es negativa
        ;; Número negativo pasa a positivo haciendo que el numerador sea positivo y teniendo en cuenta antes el posible ajuste del denominador si éste era negativo y no el numerador
        ((par (absoluto (primero(ajustar_negativo_racional(primero(segundo matriz))))))(segundo(ajustar_negativo_racional(primero(segundo matriz)))))
        ;; Número positivo pasa a negativo haciendo la resta entre 0 y el número y teniendo en cuenta antes el posible ajuste si tanto numerador como denominador era negativos
        ((par ((restaent cero) (primero(ajustar_negativo_racional(primero(segundo matriz)))))) (segundo(ajustar_negativo_racional(primero(segundo matriz)))))
        ))
      ((negativo (primero(ajustar_negativo_racional(segundo(primero matriz))))) ;; Se comprueba que el valor de la posición en la segunda fila y primera columna es negativa
       ;; Número negativo pasa a positivo haciendo que el numerador sea positivo y teniendo en cuenta antes el posible ajuste del denominador si éste era negativo y no el numerador
       ((par (absoluto (primero(ajustar_negativo_racional(segundo(primero matriz))))))(segundo(ajustar_negativo_racional(segundo(primero matriz)))))
       ;; Número positivo pasa a negativo haciendo la resta entre 0 y el número y teniendo en cuenta antes el posible ajuste si tanto numerador como denominador era negativos
        ((par ((restaent cero) (primero(ajustar_negativo_racional(segundo(primero matriz)))))) (segundo(ajustar_negativo_racional(segundo(primero matriz)))))
       ))
     ;; El valor de la posición de la primera fila y primera columna, pasa a ser el valor de la posición de la segunda fila y segunda columna
     ;; Se tiene  en cuenta si tiene algún valor negativo ya sea en el numerador o en el denominador para ajustar el resultado si es necesario
     (ajustar_negativo_racional(primero (primero matriz))))
    )
  )

;; Obtiene la matriz inversa de una matriz 
;; (test_matriz (inversa matriz_prueba1))
;; (((10 7) (2 7)) ((-8 7) (4 7)))
(define inversa
  (lambda (matriz)
    ((inversa? matriz) ;; Se comprueba si se puede realizar la inversa
     ((producto_coeficiente_matriz (inverso_racionales (determinante matriz))) (adjunta_matriz matriz)) ;; Producto del determinante de la matriz con la matriz adjunta de la matriz
     matriz ;; Se devuelve la matriz inicial si no tiene inversa
     )
    )
  )
;; Aplica un valor a cada una de las posiciones de una matriz 2x2
;; (test_matriz ((producto_coeficiente_matriz ((par dos) dos)) matriz_prueba1))
;; (((1 2) (-1 4)) ((1 1) (5 4)))
(define producto_coeficiente_matriz
  (lambda (valor)
    (lambda (matriz)
      ((((definir_matriz
           ((prod_racionales valor) (primero(primero matriz)))) ;; Producto de un valor con el valor de la primera fila y primera columna de la matriz
         ((prod_racionales valor) (primero(segundo matriz)))) ;; Producto de un valor con el valor de la primera fila y segunda columna de la matriz
        ((prod_racionales valor) (segundo(primero matriz)))) ;; Producto de un valor con el valor de la segunda fila y primera columna de la matriz
       ((prod_racionales valor) (segundo(segundo matriz)))) ;; Producto de un valor con el valor de la segunda fila y segunda columna de la matriz
      )
    )
  )

;; Realiza la potencia de una matriz mediante el algoritmo binario llamado exponenciación binaria
;; (test_matriz ((potencia_matricesaux matriz_prueba1) deux))
;; (((0 1) (7 4)) ((-7 16) (21 16)))
(define potencia_matricesaux
    (lambda (matriz)
        (lambda (num)
            ((Y (lambda (f)
                   (lambda (n)
                     ((((esigualnat n) un) ;; Comprueba si el exponente es igual a 1
                       (lambda (no_use)
                         matriz ;; Devuelve la matriz. Fin de recursividad
                         )
                       (lambda (no_use)
                         (((par? n) ;; Comprueba si el exponente es par
                           ;; Exponente par
                           (lambda (no_use1)
                             ;; Realiza el cuadrado de la matriz obtenida recursivamente haciendo que cuando se haga la llamada recursiva, el exponente se encuentre dividido entre 2
                             (cuadrado_matrices (f ((cocientenat n) deux))) 
                             )
                           ; Exponente impar
                           (lambda (no_use1)
                             ;; Realiza el producto de la matriz pasada por parámetro con la matriz obtenida recursivamente, reduciendo en una unidad el exponente cuando se haga la llamada recursiva
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
;; (test_matriz ((potencia_matrices matriz_prueba1) deux)) 
;; (((0 1) (7 4)) ((-7 16) (21 16)))
(define potencia_matrices
  (lambda (matriz)
    (lambda (num)
      (((escero num) ;; Comprueba si el exponente es cero
        (lambda (no_use) matriz) ;; Devuelve la propia matriz
        (lambda (no_use) ((potencia_matricesaux matriz) num)));; Realiza la potencia de la matriz 
       zero) 
      )
    )
  )

