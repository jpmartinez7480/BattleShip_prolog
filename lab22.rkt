;Definicion de constantes a usar

;Funcion para obtener numeros pseudoAleatorios
;Estas constantes fueron sacadas de https://en.wikipedia.org/wiki/Linear_congruential_generator
(define a 1103515245)
(define c 12345)
(define m 2147483648)
;Esta función random tuma un xn y obtiene el xn+1 de la secuencia de números aleatorios.
(define myRandom
  (lambda
    (xn)
    (remainder (+ (* a xn) c) m)
  )
)
;Cada vez que pedimos un random, debemos pasar como argumento el random anterior.

;Acá un ejemplo que permite generar una lista de números aleatorios.
;Parámetros:
;* "cuantos" indica el largo de la lista a generar.
;* "xActual" valor actual del random, se pasa en cada nivel de recursión de forma actualizada
;* "maximo" Los números generados van desde 0 hasta maximo-1
(define getListaRandom
  (lambda (cuantos xActual maximo)
    (if (= 0 cuantos)
        '()
        (let ((xNvo (myRandom xActual)))
          (cons (remainder xNvo maximo)
              (getListaRandom (- cuantos 1) xNvo maximo)
          )
        )
    )
  )
)

;lista vacia
(define null '())

;lista de barcos a usar
(define ships '(#\s #\d #\a #\s))

;funcion que busca si hay un barco en la coordenada x,y
(define buscarCoordenada
  (lambda (L x y)
    (if (null? L) #t
     (if(and (= x (car (car L))) (= y (car (cdr (car L))))) #f
        (buscarCoordenada (cdr L) x y)
     )
    )
    )
  )

;funcion que agrega un barco segun su orientacion y largo
(define AgregarSubmarino
  (lambda (ship x y orientation  L cnt)
        (cond
          ((= orientation 0)
           (if (< cnt 2)
            (AgregarSubmarino ship x (+ y 1) orientation (append L (list(list x y #\s))) (+ cnt 1))
            L
            )
          )
          ((= orientation 1)
           (if (< cnt 2)
               (AgregarSubmarino ship (+ x 1) y orientation (append L (list(list x y #\s))) (+ cnt 1))
               L
               )
           )
         )
     )
    )

(define AgregarDestructor
  (lambda (ship x y L cnt)
    (if (< cnt 1)
        (AgregarDestructor ship x y (append L (list(list x y #\d))) (+ cnt 1))
        L
        )
    )
  )

(define AgregarAcorazado
  (lambda (ship x y orientation L cnt)
    (cond
      ((= orientation 0)
       (if (< cnt 3)
           (AgregarAcorazado ship x (+ y 1) 0 (append L (list(list x y #\a))) (+ cnt 1))
           L
           )
       )
      ((=orientation 1)
       (if (cnt < 3)
           (AgregarAcorazado ship (+ x 1) y 1 (append L (list(list x y #\a))) (+ cnt 1))
           L
           )
       )
      )
    )
  )

;Funcion que devuelve una lista de barcos y sus posiciones segun el formato (x y barco)
(define agregarShips
  (lambda (t L cnt)
    (if(null? L)
       t
     (let ((randoms (getListaRandom 2 (+ cnt 1) 10)))
     (cond
       ((eq? (car L) #\d)
        (cond
          ((null? t) (agregarShips (append t (AgregarDestructor (car L) (car randoms) (car(cdr randoms))  '() 0)) (cdr L) (+ cnt 1)))
          (else (let ((x (car randoms)))
                  (let ((y (car(cdr randoms))))
                    (if (buscarCoordenada t x y) (agregarShips (append t (AgregarDestructor (car L) x y '() 0)) (cdr L) (+ cnt 1))
                        (agregarShips t L (+ cnt 1))))))
          )
        )
       ((eq? (car L) #\s)
        (cond
          ((null? t) (agregarShips (append t (AgregarSubmarino (car L) (car randoms) (car(cdr randoms)) 1 '() 0)) (cdr L) (+ cnt 1)))
          (else (let ((x (car randoms)))
                  (let ((y (car(cdr randoms))))
                    (if (buscarCoordenada t x y) (agregarShips (append t (AgregarSubmarino (car L) x y  1 '() 0)) (cdr L) (+ cnt 1))
                        (agregarShips t L (+ cnt 1))))))
          )
        )
      ((eq? (car L) #\a)
       (cond
         ((null? t) (agregarShips (append t (AgregarAcorazado (car L) (car randoms) (car(cdr randoms)) 0 '() 0)) (cdr L) (+ cnt 1)))
         (else (let ((x (car randoms)))
                 (let ((y (car(cdr randoms))))
                   (if (buscarCoordenada t x y) (agregarShips (append t (AgregarAcorazado (car L) x y 0 '() 0)) (cdr L) (+ cnt 1))
                       (agregarShips t L (+ cnt 1)))))) 
       )
      )
     )   
     )
    )
  )
  )
;Funcion que busca un barco en (x,y), si hay devuelve el barco que esta en la posicion (x,y)
(define hayBarco
  (lambda (x y L)
    (if(null? L) #\f
      (cond
       ((and (= x (car(car L))) ((= y (car(cdr(car L))))))
          (car(cdr(cdr(car L)))))
       (else (hayBarco x y (cdr L)))
     )
    )
  )
  )

(define crearColumna
  (lambda (M cnt col)
    (if (= M cnt) col
      (crearColumna (append col (cons '() '())) (+ cnt 1) col)
      )
    )
  )

(define crearMatriz
  (lambda (N M board x y)
    (if (= N x) board
      (cond
        ((< y M) (crearMatriz N M (append board (list(list x y #\-))) x (+ y 1)))
        (else (crearMatriz N M board (+ x 1) (- M y)))
        )
      )
    )
  )
       
(crearMatriz 10 10 '() 0 0)
;(append '() (list 1 (list 2 3)))
;(length (append '() (cons '() '())))
;(AgregarBarco #\d 0 1 0 '() 0)
;(agregarShips '() ships 0)
;(getListaRandom 2 1 11)
;(getListaRandom 2 2 11)
;(define tablero '((0 1 #\a) (4 4 #\a) (3 2 #\a) (1 2 #\a)))     
;(buscarCoordenada tablero 1 4)                    
