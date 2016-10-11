#lang racket
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
(define L '())

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
      ((= orientation 1)
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
  (lambda (t L N M cnt)
    (if(null? L)
       t
     (let ((x (getListaRandom 1 (+ cnt 1) (- (/ N 2) 1) )))
       (let ((y (getListaRandom 1 (+ cnt 1) (- M 1))))
     (cond
       ((eq? (car L) #\d)
        (cond
          ((null? t) (agregarShips (append t (AgregarDestructor (car L) (car x) (car y)  '() 0)) (cdr L) N M (+ cnt 1)))
          (else 
                    (if (buscarCoordenada t (car x) (car y)) (agregarShips (append t (AgregarDestructor (car L) (car x) (car y) '() 0)) (cdr L) N M (+ cnt 1))
                        (agregarShips t L N M (+ cnt 1))))
          )
        )
       ((eq? (car L) #\s)
        (cond
          ((null? t) (agregarShips (append t (AgregarSubmarino (car L) (car x) (car y) 1 '() 0)) (cdr L) N M (+ cnt 1)))
          (else 
                    (if (buscarCoordenada t (car x) (car y)) (agregarShips (append t (AgregarSubmarino (car L) (car x) (car y)  1 '() 0)) (cdr L) N M (+ cnt 1))
                        (agregarShips t L N M (+ cnt 1))))
          )
        )
      ((eq? (car L) #\a)
       (cond
         ((null? t) (agregarShips (append t (AgregarAcorazado (car L) (car x) (car y) 0 '() 0)) (cdr L) N M (+ cnt 1)))
         (else 
                   (if (buscarCoordenada t (car x) (car y)) (agregarShips (append t (AgregarAcorazado (car L) (car x) (car y) 0 '() 0)) (cdr L) N M (+ cnt 1))
                       (agregarShips t L N M (+ cnt 1)))) 
       )
      )
     )   
     )
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

;Funcion que busca un barco en (x,y), si hay, devuelve la lista sin el barco encontrado
(define removeShip
  (lambda (x y lista)
    (if (null? lista) null
        (if (and(= x (car(car lista))) (= y (car(cdr(car lista))))) (cdr lista)
           (cons (append(car lista)) (removeShip x y (cdr lista)))
          )
        )
    )
  )
        
(define crearMatriz
 (lambda (N M board x y ships)
   (if (= N x) board
      (cond
        ((and (< y M) (not(list? (assoc x ships)))) (crearMatriz N M (append board (list(list x y #\-))) x (+ y 1) ships))
        ((and (< y M) (list? (assoc x ships)))
         (if (= (car(cdr (assoc x ships))) y) (crearMatriz N M (append board (list(list x y (car(cdr(cdr(assoc x ships))))))) x (+ y 1) (removeShip x y ships))
             (crearMatriz N M (append board (list(list x y #\-))) x (+ y 1) ships)))   
        (else (crearMatriz N M board (+ x 1) (- M y) ships))
        )
      )
    )
  )

(define createBoardRL
  (lambda (N M ships seed)
    (if(list? (car ships)) (crearMatriz N M '() 0 0 ships)
       (createBoardRL N M (append L (agregarShips '() ships N M seed)) seed)
       )
    )
  )

;funcion que busca un barco en (x,y) si lo encuentra devuelve la lista sin el barco, y en la ultima pos entrega el barco
(define buscar
  (lambda (x y lista)
    (if(null? lista) null
       (if(and(= x (car(car lista))) (= y (car(cdr(car lista))))) (list (cdr lista) (car(cdr(cdr(car lista)))))
          (list* (car lista) (buscar x y (cdr lista))) 
      )
    )
  )
  )
 
      
;(char?(last(buscar 3 8 (agregarShips '() ships 10 10 0))))
;(createBoardRL 10 10 ships 0)
;(define lista (list(list 2 2 #\s)  (list 1 3 #\d) (list 1 2 #\s) (list 5 5 #\d)))
;(not(list? (assoc 6 lista)))
;(append L (agregarShips '() ships 10 10 0))    
;(createBoardRL 10 10 ships 10)
;(createBoardRL 10 10 ships 0)
;(append '() (list 1 (list 2 3)))
;(length (append '() (cons '() '())))
;(AgregarBarco #\d 0 1 0 '() 0)

;(getListaRandom 2 1 11)
;(getListaRandom 2 2 11)
;(define tablero '((0 1 #\a) (4 4 #\a) (3 2 #\a) (1 2 #\a)))     
;(buscarCoordenada tablero 1 4)                    
