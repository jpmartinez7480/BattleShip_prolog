#lang r6rs
(import (rnrs))

;Funcion para obtener numeros pseudoAleatorios
;Estas constantes fueron sacadas de https://en.wikipedia.org/wiki/Linear_congruential_generator
(define a 1103515245)
(define c 12345)
(define m 2147483648)
;Esta función random tuma un xn y obtiene el xn+1 de la secuencia de números aleatorios.
(define myRandom
  (lambda
    (xn)
    (mod (+ (* a xn) c) m)
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
          (cons (mod xNvo (truncate maximo))
              (getListaRandom (- cuantos 1) xNvo maximo)
          )
        )
    )
  )
)
(define L '())

;;; TDA positions

;Constructor
;crea una posicion en base a los argumentos x e y
;entrada:x:coordenada horizontal de la posicion,y:coordenada vertical de la posicion
;salida:lista que contiene el par ordenado x e y
(define make-position
  (lambda (x y)
  (if(and(and(number? x) (<= 0 x)) (and(number? y) (<= 0 y))) (list x y)
     '()
   )
  )
 )
;Pertenencia
;verifica que p sea de tipo position
;entrada:p:objeto a evaluar si es position
;salida:booleano que indica si es del tipo de dato position
(define position?
  (lambda (p)
  (if(and(list? p)(not(null? p)))
     (if(and(list?(car p))(not(null? (car p))))
             (if(and(<= 0 (car(car p))) (<= 0 (car(cdr(car p))))) #\t
                #\f
                )
             (if(and(<= 0 (car p)) (<= 0 (car(cdr p)))) #\t
                #\f))
     #\f
   )
 )
)

;Selectores
;obtiene la coordenada x del par ordenado x,y de position
;entrada:p:lista que contiene el par ordenado position
;salida:coordenada x de position
(define position-x
  (lambda (p)
  (if(position? p)
     (if(list? (car p)) (car(car p))
        (car p)
     )
     '()
   )
 )
)

;obtiene la coordenada y del par ordendo x,y de position
;entrada:p:lista que contiene el par ordenado position
;salida:coordenada y de position
(define position-y
  (lambda (p)
  (if(position? (car p))
     (if(list? (car p)) (car(cdr(car p)))
     (car(cdr p))
     )
     '()
   )
 )
)

;Modificadores
;Modifica el valor de x del par ordenado x,y de p
;entrada:p:lista que contiene el par ordenado x,y
;salida: nuevo lista que representa el par ordenado con el valor de x modificado
(define position-x-set
  (lambda (p x)
  (if(and(position? p)(<= 0 x)) (make-position x (position-y p))
     '()
   )
 )
)
;Modificadores
;Modifica el valor de y del par ordenado x,y de p
;entrada:p:lista que contiene el par ordenado x,y
;salida: nuevo lista que representa el par ordenado con el valor de y modificado
(define position-y-set
  (lambda (p y)
  (if(and(position? p)(<= 0 y)) (make-position (position-x p) y)
     '()
   )
 )
)


;Funciones que actuan sobre el TDA positions

;;; TDA ships

;Constructor
;crea un barco del tipo destructor en la coordenada x,y
;entrada:p:lista que contiene las coordenadas de donde ira el barco, L:lista vacia donde se guardara el barco, cnt: acumulador que indica cuantas casillas usara el barco.
;salida: lista en formato (x y barco)
(define Destructor
  (lambda (ship p L cnt)
    (if (< cnt 1)
        (Destructor ship p (append L (list(list (position-x p) (position-y p) #\d))) (+ cnt 1))
        L
        )
    )
  )
;crea un barco del tipo submarino en la coordenada x y, y puede estar vertical u horizontal.
;entrada:x:coordenada x de donde ira el barco, y:coordenada y de donde ira el barco, L:lista vacia donde se guardara el barco, cnt: acumulador que indica cuantas casillas usara el barco
;orientation:numero que indica el tipo de orientacion que tendra el barco en el tablero(1 vertical, 0 horizontal).
;salida: lista de listas que contiene las posiciones del submarino dentro del tablero,ej:((x y s)(x+1 y s))
(define Submarino
  (lambda (ship p orientation  L cnt)
        (cond
          ((= orientation 0)
           (if (< cnt 2)
            (Submarino ship p orientation (append L (list(list (position-x p) (position-y (position-y-set p (+ (position-y p) cnt))) #\s))) (+ cnt 1))
            L
            )
          )
          ((= orientation 1)
           (if (< cnt 2)
               (Submarino ship  p orientation (append L (list(list (position-x (position-x-set p (+ (position-x p) cnt))) (position-y p) #\s))) (+ cnt 1))
               L
               )
           )
         )
     )
)

;crea un barco del tipo acorazado en la coordenada x y, y puede estar vertical u horizontal.
;entrada:p: lista que contiene el par ordenado (x,y), L:lista vacia donde se guardara el barco, cnt: acumulador que indica cuantas casillas usara el barco
;orientation:numero que indica el tipo de orientacion que tendra el barco en el tablero(1 vertical, 0 horizontal).
;salida: lista de listas que contiene las posiciones del acorazado dentro del tablero,ej:((x y a)(x+1 y a)(x+2 y a))
(define acorazado
  (lambda (ship p orientation L cnt)
    (cond
      ((= orientation 0)
       (if (< cnt 3)
           (acorazado ship p 0 (append L (list(list (position-x p) (position-y (position-y-set p (+ (position-y p) cnt))) #\a))) (+ cnt 1))
           L
           )
       )
      ((= orientation 1)
       (if (cnt < 3)
           (acorazado ship p 1 (append L (list(list (position-x (position-x-set p (+ (position-x p) cnt))) (position-y p) #\a))) (+ cnt 1))
           L
           )
       )
      )
    )
  )

;Pertenencia
;verifica que listaShip sea de tipo ship.
;entrada:listaShip:objeto a evaluar si es ship o no.
;salida: booleano que indica la pertenencia al tipo de dato.
(define ship?
  (lambda (listaShip)
  (if(and((<= 0 (ship-x listaShip)) (<= 0 (ship-y listaShip)) (char? (ship-c listaShip)))) #\t
     #\f
   )
 )
)

;Selectores
;obtiene la coordenada x del barco
;entrada:s:lista que contiene las coordenadas del barco,y el barco
;salida:coordenada x del barco
(define ship-x
  (lambda (s)
  (if(ship? s) (car s)
     '()
   )
 )
)

;obtiene la coordenada y del barco
;entrada:s:lista que contiene las coordendas, y el barco.
;salida:coordenada y del barco
(define ship-y
  (lambda (s)
  (if(ship? s) (car(cdr s))
     '()
   )
 )
)

;obtiene el barco que se encuetra en la posicion x,y
;entrada: s:lista que contiene las coordenadas x,y, y el barco
;salida: caracter que indica el barco
(define ship-c
  (lambda (s)
  (if(ship? s) (car(cdr(cdr s)))
     '()
   )
 )
)
;Modificadores

;Otras funciones que actuan sobre el TDA ships
;Funcion que crea una lista de ships
(define agregarShips
  (lambda (t L N M cnt)
    (if(null? L)
       t
     (let ((x (getListaRandom 1 (+ cnt 1) (- (/ N 2) 1) )))
       (let ((y (getListaRandom 1 (+ cnt 1) (- M 1))))
         (let ((p (make-position (car x) (car y))))
     (cond
       ((eq? (car L) #\d)
        (cond
          ((null? t) (agregarShips (append t (Destructor (car L) p  '() 0)) (cdr L) N M (+ cnt 1)))
          (else 
                    (if (buscarCoordenada t p) (agregarShips (append t (Destructor (car L) p '() 0)) (cdr L) N M (+ cnt 1))
                        (agregarShips t L N M (+ cnt 1))))
          )
        )
       ((eq? (car L) #\s)
        (cond
          ((null? t) (agregarShips (append t (Submarino (car L) p 1 '() 0)) (cdr L) N M (+ cnt 1)))
          (else 
                    (if (buscarCoordenada t p) (agregarShips (append t (Submarino (car L) p 1 '() 0)) (cdr L) N M (+ cnt 1))
                        (agregarShips t L N M (+ cnt 1))))
          )
        )
      ((eq? (car L) #\a)
       (cond
         ((null? t) (agregarShips (append t (acorazado (car L) p 0 '() 0)) (cdr L) N M (+ cnt 1)))
         (else 
                   (if (buscarCoordenada t p) (agregarShips (append t (acorazado (car L) p 0 '() 0)) (cdr L) N M (+ cnt 1))
                       (agregarShips t L N M (+ cnt 1)))) 
       )
      )
     )   
     )
    )
  )
    )
     )
  )

(define removeShip
  (lambda (p lista)
    (if (null? lista) '()
        (if (and(= (position-x p) (car(car lista))) (= (position-y p) (car(cdr(car lista))))) (cdr lista)
           (cons (append(car lista)) (removeShip p (cdr lista)))
          )
        )
    )
  )
;funcion que busca un barco en (x,y) si lo encuentra devuelve la lista sin el barco, y en la ultima pos entrega el barco
(define buscar
  (lambda (p lista)
    (if(null? lista) '()
       (if(and(= (position-x p) (car(car lista))) (= (position-y p) (car(cdr(car lista))))) (append (cdr lista) (list(car(cdr(cdr(car lista))))))
          (append (list(car lista)) (buscar p (cdr lista))) 
      )
    )
  )
 )

;funcion que busca si hay un barco en la coordenada x,y
(define buscarCoordenada
  (lambda (L p)
    (if (null? L) #t
     (if(and (= (position-x p) (car (car L))) (= (position-y p) (car (cdr (car L))))) #f
        (buscarCoordenada (cdr L) p)
     )
    )
    )
  )

    
;;; TDA Board

;Constructor
;crea una lista con coordenadas y un simbolo que representa mar o el caracter de un barco
;entrada: N: cantidad de filas del tablero, M: cantidad de columnas del tablero, ships: lista con los barcos a ingresar.
;salida: lista que contiene listas que representan el tablero segun el formato (x y mar(barco).
(define crearMatrizC
 (lambda (N M board p ships)
   (if (= N (position-x p)) board
           (cond
             ((and (< (position-y p) M) (not(null? ships)))
              (if (not(char?(car(reverse(buscar p ships)))))
                  (crearMatrizC N M (append board (list(list (position-x p) (position-y p) #\-))) (make-position (position-x p) (+ (position-y p) 1)) ships)
                    (crearMatrizC N M (append board (list(list (position-x p) (position-y p) (car(reverse(buscar p ships)))))) (make-position (position-x p) (+ (position-y p) 1)) (removeShip p ships))))
             ((and (< (position-y p) M) (null? ships))
              (crearMatrizC N M (append board (list(list (position-x p) (position-y p) #\-))) (make-position (position-x p) (+ (position-y p) 1)) ships))   
             (else (crearMatrizC N M board (make-position (+ (position-x p) 1) (- M (position-y p))) ships))
           )
      )
    )
  )

(define crearMatrizL
  (lambda (N M p ships)
    (if(= N (position-x p)) '()
       (cond
         ((and (< (position-y p) M) (not(null? ships)))
          (if (not(char?(car(reverse(buscar p ships)))))
              (cons (list (position-x p) (position-y p) #\-) (crearMatrizL N M (make-position (position-x p) (+ (position-y p) 1)) ships))
              (cons (list (position-x p) (position-y p) (car(reverse(buscar p ships)))) (crearMatrizL N M (make-position (position-x p) (+ (position-y p) 1)) (removeShip p ships)))))
         ((and (< (position-y p) M) (null? ships))
          (cons (list (position-x p) (position-y p) #\-) (crearMatrizL N M (make-position (position-x p) (+ (position-y p) 1)) ships)))
         (else (crearMatrizL N M (make-position (+ (position-x p) 1) (- M (position-y p))) ships))
         )
       )
    )
  )
    
;crea una lista de listas que representa las posiciones de un tablero usando el formato (x y mar(barco)), utilizando recursion de cola
;entrada: N: cantidad de filas del tablero, M: cantidad de columnas del tablero, ships: lista con los barcos a ingresar, seed: semilla actualizable para colocar aleatoriamente un barco.
;salida: lista de listas que representa un tablero.
(define createBoardRC
  (lambda (N M ships seed)
    (let ((p (make-position 0 0)))
    (if(list? (car ships)) (crearMatrizC N M '() p ships)
       (createBoardRC N M (append L (agregarShips '() ships N M (+ seed 1))) (+ seed 1))
       )
    )
  )
  )

(define createBoardRL
  (lambda (N M ships seed)
    (let ((p (make-position 0 0)))
      (if(list? (car ships)) (crearMatrizL N M p ships)
         (createBoardRL N M (append L (agregarShips '() ships N M (+ seed 1))) (+ seed 1))
      )
    )
  )
)

;Pertenencia
;Verifica que tablero sea del tipo board
;entrada:tablero:objeto a evaluar si es board valido o no
;salida: valor booleano que indica su validez
(define board?
  (lambda (tablero)
  (if(and(list? (car tablero)) (number? (car(car tablero))) (number? (car(cdr(car tablero)))) (char? (car(cdr(cdr(car tablero)))))) #\t
     #\f
     )
  )
 )

;Selectores
;Obtiene una lista dentro de la lista board
;entrada:b:lista de listas que contiene la configuracion del tablero
;salida:primer elemento de la lista de board
(define casilla-board
  (lambda (b)
  (if(board? b) (car b)
     '()
     )
  )
 )
;Obtiene la coordenada x del trio (x y mar(barco))
;entrada: lista que contiene el trio (x y mar(barco))
;salida: numero que representa coordenada x del trio obtenido
(define coordenada-x
  (lambda (lista)
  (if(board? lista) (car(car lista))
     (if(list? lista) (car lista)
        '()
        )
   )
 )
)

;Obtiene la coordenada y del trio
;entrada:lista que contiene el trio
;salida: coordenada y del trio
(define coordenada-y
  (lambda (b)
  (if(board? b) (car(cdr (car b)))
     (if(list? b) (car(cdr b))
     '())
   )
  )
 )
;Obtiene el caracter que indica si hay mar o un barco en el trio (x y mar(barco))
;entrada: lista que contiene el trio
;salida: caracter que indica si hay mar o un barco 
(define caracter
  (lambda (lista)
  (if(board? lista) (car(cdr(cdr lista)))
     (if(list? lista) (car(cdr(cdr lista)))
        '()
        )
     )
   )
  )

;Obtiene la cantidad de filas del tablero.
;entrada:b:lista de listas que contiene la configuracion del tablero.
;salida:entero que indica la cantidad de filas del tablero.
(define getRows
  (lambda (b)
  (if(board? b) (coordenada-x (reverse b))
     '()
     )
   )
 )

;Obtiene la cantidad de columnas del tablero.
;entrada:b:lista de listas que contiene la configuracion del tablero
;salida:cantidad de columnas que contiene el tablero.
(define getCol
  (lambda (b)
  (if(board? b) (coordenada-y(reverse b))
     '()
     )
  )
 )

;Modificadores
;coloca un barco en la mitad del tablero del usuario
;entrada:board:lista de listas que contiene la configuracion del tablero,position:coordenada donde ira el barco, ship: barco a poner.
;salida:lista de listas que contiene el barco puesto en la coordenada indicada
(define putShip
  (lambda (board position ship)
    (if(and(board? board) (position? position))
       (if(not(null? position))
          (if(and(=(coordenada-x board) (position-x position)) (=(coordenada-y board) (position-y position)))
             (append(list(list (position-x position) (position-y position) ship)) (putShip (cdr board) (cdr position) ship))
            (append (list(car board)) (putShip (cdr board) position ship)))
          board)
       board
       )
    )
  )
          
;Otras funciones que actuan en el TDA
(define cantDestructores
 (lambda (b n cnt cant)
    (if(board? b)
       (if(cnt < n)
          (if(eq?(caracter b)(#\d)) (cantDestructores (cdr b) n (+ cnt 1) (+ cant 1))
             (cantDestructores (cdr b) n (+ cnt 1) cant)
             )
          cant
          )
       '()
      )
   )
  )

;funcion que verifica si el tablero entregado es valido para los parametros del juego
;entrada:board:lista de listas que contiene la configuracion del tablero
;salida:booleano que indica si se trata de un tablero valido
(define checkBoard
  (lambda (board)
    (if(null? board) #\f
          (if(not(even?(coordenada-x (reverse board)))) #\t
             #\f
             )
          )
    )
  )

;Funcion que recibe un tablero y lo convierte en un string
;entrada:board:lista de listas que contiene la configuracion del tablero
;salida:string que contiene la configuracion del tablero
(define board->string
  (lambda (board showComplete)
    (let((x (coordenada-y (reverse board))))
      (fold-left
       (lambda (base argumento)
         (if(= x (car(cdr argumento))) (string-append base "\n")
            (cond
              ((= showComplete 1)
               (string-append base " " (make-string 1 (caracter argumento))))
              ((= showComplete 0)
               (if(< (car argumento) (/ (+ x 1) 2))
                  (string-append base " " (make-string 1 #\-))
                  (string-append base " " (make-string 1 (caracter argumento)))
          )
        )
            )
         )
         )
       (string-append " " (make-string 1 (caracter(casilla-board board))))
       (cdr board)
       )
      )
    )
  )

;funcion que permite realizar una jugada sobre el tablero
;entrada:board:lista de listas que contiene la configuracion del tablero,ship:barco del usuario
;que realiza el disparo, positions: lista de posiciones donde caera el disparo, seed: semilla para generar
;un ataque del pc
;salida:nuevo tablero con las jugadas realizadas y un entero que indique el resultado del ataque.
(define jugada
  (lambda (board positions seed)
    (if(not(null? positions))
       (if(position? (car positions))
          (if(and (= (coordenada-x board) (position-x (car positions)))
                  (=(coordenada-y board) (position-y (car positions))))
             (cond
               ((eq? #\- (caracter(casilla-board board)))
                   (if(= (length positions) 1)
                      (append (list(list(coordenada-x board) (coordenada-y board) #\x)) (jugada (cdr board) (cdr positions) seed) );jugada
                      (append (list(list(coordenada-x board) (coordenada-y board) #\x)) (jugada (cdr board)  (cdr positions) seed))
                    ))
               ((eq? #\d (caracter(casilla-board board)))
                (if(= (length positions) 1) 
                   (append (list(list(coordenada-x board) (coordenada-y board) #\x )) (jugada (cdr board)  (cdr positions) seed) );jugada
                   (append (list(list(coordenada-x board) (coordenada-y board) #\x )) (jugada (cdr board)  (cdr positions) seed))
                   ))
                
               ((eq? #\s (caracter(casilla-board board)))
                (if(= (length positions) 1) 
                   (append (list(list(coordenada-x board) (coordenada-y board) #\x )) (jugada (cdr board) (cdr positions) seed) );jugada
                   (append (list(list(coordenada-x board) (coordenada-y board) #\x )) (jugada (cdr board) (cdr positions) seed))
                   ))
                
               ((eq? #\a (caracter(casilla-board board)))
                (if(= (length positions) 1) 
                   (append (list(list(coordenada-x board) (coordenada-y board) #\x )) (jugada (cdr board) (cdr positions) seed) );juagda
                   (append (list(list(coordenada-x board) (coordenada-y board) #\x )) (jugada (cdr board)  (cdr positions) seed))
                   )
                )
               )
               
               
             (append (list(list(coordenada-x board) (coordenada-y board) (caracter (casilla-board board)))) (jugada (cdr board) positions seed))
          )
       ) 
       board
       )
   )
  )

(define play
  (lambda (board ship positions seed)
       ;(prepareToAttackCom (jugada board positions (+ 21124 seed)) seed '())
    (prepareToAttackCom (prepareToAttackUser board ship positions '() (+ seed 2123)) seed '() positions)
    )
  )
  
(define prepareToAttackUser
  (lambda (board ship  positions b seed)
    (if(null? b)
       (let ((pos (positionAttack ship seed board '() positions 1)))
         (prepareToAttackUser board ship positions (append b (jugada board pos (+ 24234 seed))) seed)
      )
       b
    )
  )
  )
                 
;funcion que realiza las acciones necesarias para que la cpu haga su ataque
;entrada: board:lista de listas que contiene la configuracion del tablero, seed:semilla que permite actualizar el random,
;salida:
(define prepareToAttackCom
  (lambda (board seed L positions)
    (let ((barcos (barcosActuales board 0 (/ (length board) 2) '())))
      (let((ship (chooseShipToAttack barcos (+ seed 1234))))
         (let ((positions (positionAttack ship (+ seed 2312) board '() positions 0)))
                (jugada board positions (+ 1242445 seed))
               )
            )
          )
         )
      
    )

      

     
(define positionAttack 
  (lambda (ship seed board L positions n)
    (if(null? L)
       (if(board? board)
          (cond 
            ((eq? ship #\d)
             (let(( x (getListaRandom 1 (+ 2435 seed) (/ (+ (getRows board) 1) 2))))
               (let (( y (getListaRandom 1 (+ seed 732723) (getCol board))))
                 (if(= n 0)
                 (positionAttack ship seed board (append  L (list(make-position (+ (car x) 5) (car y)))) positions 0)
                 (positionAttack ship seed board (append  L (list(make-position (position-x positions) (position-y positions)))) positions 1)
                 )
             )
           )
          )
            ((eq? ship #\s)
             (let(( x (getListaRandom 1 (+ 2235 seed) (/ (+ (getRows board) 1) 2))))
               (let (( y (getListaRandom 1 (+ seed 2723) (getCol board))))
                 (if(= n 0)
                    (positionAttack ship seed board (append  L (AtaqueSubmarino board (+ (car x) 5) (car y) 0 '() 0)) positions 0)
                    (positionAttack ship seed board (append  L (AtaqueSubmarino board (position-x positions) (position-y positions) 0 '() 1)) positions 1)
                    )
               )
             ))
            ((eq? ship #\a)
             (let((x (getListaRandom 1 (+ 2235 seed) (/ (+ (getRows board) 1) 2))))
                (let (( y (getListaRandom 1 (+ seed 2723) (getCol board))))
                  (if(= n 0)
                     (positionAttack ship seed board (append  L (AtaqueAcorazado board (+ (car x) 5) (car y) 0 '())) positions n)
                     (positionAttack ship seed board (append  L (AtaqueAcorazado board (position-x positions) (position-y positions) 0 '())) positions n)
                     )
            )
          )
          )
            )
            )
       L
          )
      
    )
   )
  
  

(define AtaqueSubmarino
  (lambda(board x y cnt L n)
    (if(< cnt 2)
       (cond
         ((= n 1)
          (cond
            ((= cnt 0)
             (if(>= (+ x 1)(/(+(getRows board)1)2))
             (AtaqueSubmarino board x y (+ 1 cnt) (append L (list(make-position (- x 1) y))) n)
             (AtaqueSubmarino board x y (+ 1 cnt) (append L (list(make-position  x y))) n)))
            ((= cnt 1)
             (if(>= (+ x 1)(/(+(getRows board)1)2))
                (AtaqueSubmarino board x y (+ 1 cnt) (append L (list(make-position  x y))) n)
                (AtaqueSubmarino board x y (+ 1 cnt) (append L (list(make-position (+ x 1) y))) n)))
             ))
         ((= n 0)
          (cond
            ((= cnt 0)
               (if(> (+ x 1) (getRows board))
                  (AtaqueSubmarino board x y (+ 1 cnt) (append L(list(make-position (- x 1) y))) n)
                  (AtaqueSubmarino board x y (+ 1 cnt) (append L(list(make-position x y))) n)))
            ((= cnt 1)
             (if(> (+ x 1) (getRows board))
                (AtaqueSubmarino board x y (+ 1 cnt) (append L(list(make-position x y))) n)
                (AtaqueSubmarino board x y (+ 1 cnt) (append L(list(make-position (+ x 1) y))) n)))
             )
          )
         )
       L
       )
    )
  )

(define AtaqueAcorazado
  (lambda(board x y cnt L)
    (if(< cnt 2)
       (cond
         ((= cnt 0)
          (if(>(+ y 1) (getCol board))
           (AtaqueAcorazado board x y ( + 1 cnt) (append L (list(make-position x (- y 1)))))
            (AtaqueAcorazado board x y (+ 1 cnt) (append L (list(make-position x y))))
            ))
         ((= cnt 1)
          (if(>(+ y 1) (getCol board))
             (AtaqueAcorazado board x y (+ 1 cnt) (append L(list(make-position x y))))
             (AtaqueAcorazado board x y (+ 1 cnt) (append L(list(make-position x (+ y 1)))))
             ))
         )
       L
       )
    )
  )
             
(define barcosActuales
  (lambda (board cnt dim L)
    (if(< cnt dim)
       (if(not(eq? (caracter(casilla-board board)) #\-))
          (barcosActuales (cdr board) (+ cnt 1) dim (append L (list(caracter(casilla-board board)))))
          (barcosActuales (cdr board) (+ cnt 1) dim L)
          )
       L
       )
    
    )
  )

(define chooseShipToAttack
  (lambda (ships seed)
    (let ((s (getListaRandom 1 (+ seed 1) (length ships))))
      (getByPos ships (car s))
      )
    )
  )
      
(define getByPos
  (lambda (ships i)
    (if(= i 0) (car ships)
       (getByPos (cdr ships) (- i 1))
       )
    )
  )
             
(define ships '(#\s #\d #\a #\s))
(define b (createBoardRC 10 10 ships 0))
(define bc (createBoardRL 10 10 ships 0))
(define p (list(make-position 7 0)))
(define attack (list(make-position 4 2)))
(define aCpu (chooseShipToAttack(barcosActuales b 0 50 '()) 54))
;(define h '((1 2)(3 4)))

