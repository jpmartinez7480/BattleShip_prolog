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
(define ships '(#\s #\d))

;funcion que agrega un barco segun su orientacion y largo
(define AgregarBarco
  (lambda (ship x y orientation  L cnt)
    (if (eq? #\s ship)
        (cond
          ((= orientation 0)
           (if (< cnt 2)
            (AgregarBarco ship x (+ y 1) orientation (append L (list(list x y #\s))) (+ cnt 1))
            L
            )
          )
         )
     (if (eq? #\d ship)
         (if (< cnt 1)
         (AgregarBarco ship x y orientation (append L (list(list x y #\d))) (+ cnt 1))
         L
         )
       )
     )
    )
  )
               
            

;funcion que buscar si hay un barco en la coordenada x,y
(define buscarCoordenada
  (lambda (L x y)
    (if (null? L) #t
     (if(and (= x (car (car L))) (= y (car (cdr (car L))))) #f
        (buscarCoordenada (cdr L) x y)
     )
    )
    )
  )

(define agregarShips
  (lambda (t L cnt)
    (if(null? L)
       t
     (let ((randoms (getListaRandom 2 (+ cnt 1) 10)))
     (agregarShips (append t (AgregarBarco (car L) (car randoms) (car(cdr randoms)) 0  '() 0 )) (cdr L) (+ cnt 1))    
     )
    )
  )
)

;(AgregarBarco #\d 0 1 0 '() 0)
(agregarShips '() ships 0)
;(getListaRandom 2 0 11)
;(getListaRandom 2 1 11)
;(getListaRandom 2 2 11)
;(define tablero '((0 1 #\a) (4 4 #\a) (3 2 #\a) (1 2 #\a)))     
;(buscarCoordenada tablero 1 4)                    
