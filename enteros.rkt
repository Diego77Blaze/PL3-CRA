ochosieteseiscinco; Booleanos. Son los únicos lambda términos no currificados.

(define true (lambda (x y) x))

(define false (lambda (x y) y))

(define neg (lambda (x) (x false true)))

(define and (lambda (x y) (x y false)))

(define or (lambda (x y) (x true y)))

; Pares ordenados

(define par (lambda (x)
              (lambda (y)
                (lambda (f) (f x y)))))

(define primero (lambda (p) (p true)))

(define segundo (lambda (p) (p false)))

;;;;; Combinador de punto fijo

(define Y
  (lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v)))))))

;;;;;; Orden en naturales y test de nulidad

(define esmenoroigualnat (lambda (n)
                             (lambda (m)
                                (escero ((restanat n) m)))))

(define esmayoroigualnat (lambda (n)
                            (lambda (m)
                               (escero ((restanat m) n)))))

(define esmenornat (lambda (n)
                     (lambda (m)
                       (and ((esmenoroigualnat n) m) (noescero ((restanat m) n))))))

(define esmayornat (lambda (n)
                     (lambda (m)
                       (and ((esmayoroigualnat n) m) (noescero ((restanat n) m))))))

(define esigualnat (lambda (n)
                     (lambda (m)
                       (and ((esmayoroigualnat n) m) ((esmenoroigualnat n) m)))))

(define escero (lambda (n)
                 ((n (lambda (x) false)) true)))

(define noescero (lambda (n)
                    (neg (escero n))))

; Aritmética natural. Se define también "comprobar" para poder hacer pruebas. Se definen algunos naturales para hacer comprobaciones. Se escriben en francés para distinguirlos de los enteros
; que se escribirán en español.

(define cero (lambda (f)
               (lambda (x) x)))

(define sucesor (lambda (n)
                  (lambda (f)
                    (lambda (x)
                     (f((n f) x))))))

(define uno (sucesor cero))

(define dos (sucesor uno))

(define tres (sucesor dos))

(define cuatro (sucesor tres))

(define cinco (sucesor cuatro))

(define seis (sucesor cinco))

(define siete (sucesor seis))

(define ocho (sucesor siete))

(define nueve (sucesor ocho))

(define diez (sucesor nueve))

(define once (sucesor diez))

(define doce (sucesor once))

(define trece (sucesor doce))

(define catorce (sucesor trece))

(define quince (sucesor catorce))

(define diez-seis (sucesor quince))

(define diez-siete (sucesor diez-seis))

(define diez-ocho (sucesor diez-siete))

(define diez-nueve (sucesor diez-ocho))

(define vingt (sucesor diez-nueve))

;; Comprobar

(define comprobar (lambda (n)
                    ((n (lambda (x) (+ 1 x))) 0)))

;; Suma naturales

(define sumnat (lambda (n)
                 (lambda (m)
                   ((n (lambda (x) (sucesor x))) m))))

;; Producto naturales

(define prodnat (lambda (n)
                   (lambda (m)
                     (lambda (f)
                       (lambda (x) ((m (n f)) x))))))

(define prefn (lambda (f)
                (lambda (p)
                  ((par (f (primero p))) (primero p)))))

;; Predecesor y resta

(define predecesor (lambda (n)
                     (lambda (f)
                       (lambda (x)
                            (segundo ((n ((lambda (g)
                                             (lambda (p) ((prefn g) p))) f)) ((par x) x)))))))

(define restanat (lambda (n)
                     (lambda (m)
                        ((m (lambda (x) (predecesor x))) n))))

;; Resto de la división euclídea. Si el divisor es cero, devuelve false.

(define restonataux
    (lambda (n)
        (lambda (m)
            ((Y (lambda (f)
                 (lambda (x)
                    ((((esmayoroigualnat x) m)
                        (lambda (no_use)
                            (f ((restanat x) m))
                        )
                        (lambda (no_use)
                            x
                        )
                    )
                        cero)    ; Pasa cero como argumento de no_use
                )
            ))
                n)  ; Pasa n como el valor inicial de x.
        )
))

(define restonat (lambda (n)
                      (lambda (m)
                        (((escero m) (lambda (no_use) false) (lambda (no_use) ((restonataux n) m))) cero))))

;; Cociente de la división euclídea. Al igual que el resto, devuelve false si se divide por cero.

(define cocientenataux
    (lambda (n)
        (lambda (m)
            ((Y (lambda (f)
                (lambda (x)
                    ((((esmayoroigualnat x) m)
                        (lambda (no_use)
                            (sucesor (f ((restanat x) m)))
                        )
                        (lambda (no_use)
                            cero
                        )
                    )
                        cero)    ; Pasa cero como argumento de no_use
                )
            ))
                n)  ; Pasa n como el valor inicial de x.
        )
    )
)

(define cocientenat (lambda (n)
                      (lambda (m)
                        (((escero m) (lambda (no_use) false) (lambda (no_use) ((cocientenataux n) m))) cero))))

;; Máximo común denominador.

(define mcdnat
    (lambda (n)
        (lambda (m)
            (((Y (lambda (f)
                   (lambda (x)
                     (lambda(y)
                      (((escero y)
                       (lambda (no_use)
                            x
                        )
                       (lambda (no_use)
                            ((f y)((restonat x) y))
                        )

                    )
                        cero)    ; Pasa cero como argumento de no_use
                ))
            ))
                n) ; Pasa n como el valor inicial de x.
          m)       ; Pasa m como el valor inicial de y.
    )
))

;;;; Paridad

(define par? (lambda (n)
               (escero ((restonat n) dos))))

(define cuadrado (lambda (n)
                   ((prodnat n) n)))


;;;;; Potencias de naturales usando algo binario.

(define potencianat
    (lambda (n)
        (lambda (m)
            ((Y (lambda (f)
                (lambda (y)
                    (((escero y)
                        (lambda (no_use)
                            uno
                        )
                        (lambda (no_use)
                          (((par? y)
                           (lambda (no_use1)
                             (cuadrado (f ((cocientenat y) dos))))
                           (lambda (no_use1)
                             ((prodnat n) (f (predecesor y))))) cero)
                        )
                    )
                        cero)    ; Pasa cero como argumento de no_use
                )
            ))
                m)  ; Pasa n como el valor inicial de x.
        )
    )
)

;;;;;; Definición de algunos enteros. Se codifican los enteros mediante pares de naturales: el par (m,n) es una representación de m-n. Es obvio que varios
;;;;;; pares codifican el mismo entero. Por ejemplo, (7,5)=(9,7). Por lo tanto, los enteros se definen como el conjunto cociente de NxN mediante la relación
;;;;;; de equivalencia R dada por
;;;;;;
;;;;;;                     (m,n) R (m',n') si y solo si m-n=m'-n'

(define cero ((par cero) cero))

(define -uno ((par cero) uno))

(define -dos ((par cero) dos))

(define -tres ((par cero) tres))

(define -cuatro ((par cero) cuatro))

(define -cinco ((par cero) cinco))

(define -seis ((par cero) seis))

(define -siete ((par cero) siete))

(define -ocho ((par cero) ocho))

(define -nueve ((par cero) nueve))

(define -diez ((par cero) diez))

(define -once ((par cero) once))

(define -doce ((par cero) doce))

(define -trece ((par cero) trece))

(define -catorce ((par cero) catorce))

(define -quince ((par cero) quince))

(define -diez-seis ((par cero) diez-seis))

(define -diecisiete ((par cero) diez-siete))

(define -dieciocho ((par cero) diez-ocho))

(define -diecinueve ((par cero) diez-nueve))

(define -veinte ((par cero) vingt))

(define uno ((par uno) cero))

(define dos ((par dos) cero))

(define tres ((par tres) cero))

(define cuatro ((par cuatro) cero))

(define cinco ((par cinco) cero))

(define seis ((par seis) cero))

(define siete ((par siete) cero))

(define ocho ((par ocho) cero))

(define nueve ((par nueve) cero))

(define diez ((par diez) cero))

(define once ((par once) cero))

(define doce ((par doce) cero))

(define trece ((par trece) cero))

(define catorce ((par catorce) cero))

(define quince ((par quince) cero))

(define diez-seis ((par diez-seis) cero))

(define diecisiete ((par diez-siete) cero))

(define dieciocho ((par diez-ocho) cero))

(define diecinueve ((par diez-nueve) cero))

(define veinte ((par vingt) cero))

;;;;; Orden, valor absoluto y tests de nulidad, positividad y negatividad.
;;;
;;; m-n > m'-n' si y solo si m+n' > m'+n e igual con el resto

(define esmayoroigualent (lambda (r)
                           (lambda (s)
                             ((esmayoroigualnat ((sumnat (primero r)) (segundo s))) ((sumnat (primero s)) (segundo r))))))

(define esmenoroigualent (lambda (r)
                           (lambda (s)
                             ((esmenoroigualnat ((sumnat (primero r)) (segundo s))) ((sumnat (primero s)) (segundo r))))))

(define esmayorent (lambda (r)
                           (lambda (s)
                             ((esmayornat ((sumnat (primero r)) (segundo s))) ((sumnat (primero s)) (segundo r))))))

(define esmenorent (lambda (r)
                           (lambda (s)
                             ((esmenornat ((sumnat (primero r)) (segundo s))) ((sumnat (primero s)) (segundo r))))))

(define esigualent (lambda (r)
                           (lambda (s)
                             ((esigualnat ((sumnat (primero r)) (segundo s))) ((sumnat (primero s)) (segundo r))))))

(define absoluto (lambda (r)
                    (((esmayoroigualnat (primero r)) (segundo r)) ((par ((restanat (primero r)) (segundo r))) cero) ((par ((restanat (segundo r)) (primero r))) cero))))

(define negativo (lambda (r)
                   ((esmenorent r) cero)))

(define positivo (lambda (r)
                   ((esmayorent r) cero)))

(define esceroent (lambda (r)
                     ((esigualnat (primero r)) (segundo r))))

(define noesceroent (lambda (r)
                       (neg (esceroent r))))

;;;;; Reducción a representante canónico de la clase de equivalencia.

(define reducir (lambda (r)
                  (((esmayoroigualnat (primero r)) (segundo r))
                        ((par ((restanat (primero r)) (segundo r))) cero)
                        ((par cero) ((restanat (segundo r)) (primero r))))))

;;;;; Aritmética entera. La respuesta está siempre dada por el representante canónico de la clase de equivalencia.

(define testenteros (lambda (r)
                      (- (comprobar (primero r)) (comprobar (segundo r)))))

(define sument (lambda (r)
                  (lambda (s)
                    (reducir ((par ((sumnat (primero r)) (primero s))) ((sumnat (segundo r)) (segundo s)))))))

(define prodent (lambda (r)
                  (lambda (s)
                    (reducir ((par ((sumnat ((prodnat (primero r)) (primero s))) ((prodnat (segundo r)) (segundo s))))
                          ((sumnat ((prodnat (primero r)) (segundo s))) ((prodnat (segundo r)) (primero s))))))))

(define restaent (lambda (r)
                   (lambda (s)
                     (reducir ((par ((sumnat (primero r)) (segundo s))) ((sumnat (segundo r)) (primero s)))))))

(define opuesto (lambda (r)
                  ((par (segundo r)) (primero r))))

;; Lo siguiente reduce la división de enteros a división de naturales. Si m mayor o igual que 0 y n > 0, y si q y r son cociente y resto de la división de m entre n, se tiene
;;  m  = q       * n        + r
;;  m  = (-q)    * (-n)     + r
;; -m  = (-(q+1))* n        + (n-r)
;; -m  = (q+1)   * (-n)     + (n-r),
;; siempre y cuando el resto no sea cero. Cuando el divisor es cero, la función cocienteent devuelve false.

(define cocienteent_aux (lambda (r)
                          (lambda (s)
                            ((cocientenat (primero (absoluto r))) (primero (absoluto s))))))

; Caso1: resto cero. Si m= q*n, entonces -m= (-q)*n, -m = q* (-n) y m= (-q)*(-n).

(define cocienteentaux-caso1 (lambda (r)
                               (lambda (s)
                                  ((or (and ((esmayoroigualent r) cero) (positivo s)) (and (negativo r) (negativo s))) ((par ((cocientenat (primero (absoluto r))) (primero (absoluto s)))) cero)
                                                                                                                       ((par cero) ((cocientenat (primero (absoluto r))) (primero (absoluto s))))))))

; Caso 2: resto no nulo

(define cocienteentaux-caso2 (lambda (r)
                                (lambda (s)
                                    (((esmayoroigualent r) cero) ((positivo s) ((par ((cocienteent_aux r) s)) cero) ((par cero) ((cocienteent_aux r) s)))
                                                                 ((positivo s) ((par cero) (sucesor ((cocienteent_aux r) s))) ((par (sucesor ((cocienteent_aux r) s))) cero))))))
; Cociente cuando no hay división por cero

(define cocienteentaux (lambda (r)
                         (lambda (s)
                           ((escero ((restonat (primero (absoluto r))) (primero (absoluto s)))) ((cocienteentaux-caso1 r) s) ((cocienteentaux-caso2 r) s)))))

; Cociente considerando la división por cero

(define cocienteent (lambda (r)
                      (lambda (s)
                        (((esceroent s) (lambda (no_use) false) (lambda (no_use) ((cocienteentaux r) s))) cero))))

; Resto. Si se divide por cero, devuelve false

(define restoentaux1 (lambda (r)
                        (lambda (s)
                          ((or (and ((esmayoroigualent r) cero) (positivo s)) (and ((esmayoroigualent r) cero) (negativo s))) ((par ((restonat (primero (absoluto r))) (primero (absoluto s)))) cero)
                                                                                                           ((par ((restanat (primero (absoluto s)))((restonat (primero (absoluto r))) (primero (absoluto s))))) cero)))))

(define restoentaux (lambda (r)
                       (lambda (s)
                          ((escero ((restonat (primero (absoluto r))) (primero (absoluto s)))) cero ((restoentaux1 r) s)))))

(define restoent (lambda (r)
                      (lambda (s)
                        (((esceroent s) (lambda (no_use) false) (lambda (no_use) ((restoentaux r) s))) cero))))

;; Como mcd (r,s)=mcd(|r|,|s|), se tiene

(define mcdent (lambda (r)
                 (lambda (s)
                   ((par ((mcdnat (primero (absoluto r))) (primero (absoluto s)))) cero))))

;; Mínimo común múltiplo

(define mcment (lambda (r)
                 (lambda (s)
                   ((cocienteent ((prodent r) s)) ((mcdent r) s)))))
