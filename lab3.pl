%Representacion:
%[[x1,y1,mar o barco],...,[xn,yn,mar o barco]]
% el tablero quedara representado en el juego como una lista de listas,
% donde cada lista corresponde a la representacion de una casilla, dada
% por: coordenada x(natural), coordenada y(natural) y un caracter que
% indique si hay un barco, mar o cayo un disparo.
tablero(0,6,6,6,[[0,0,a],[0,1,x],[0,2,x],[0,3,x],[0,4,x],[0,5,x],
[1,0,x],[1,1,x],[1,2,x],[1,3,x],[1,4,s],[1,5,x],
[2,0,x],[2,1,x],[2,2,d],[2,3,x],[2,4,x],[2,5,x],
[3,0,x],[3,1,x],[3,2,x],[3,3,x],[3,4,d],[3,5,x],
[4,0,x],[4,1,s],[4,2,x],[4,3,x],[4,4,x],[4,5,x],
[5,0,x],[5,1,x],[5,2,x],[5,3,x],[5,4,x],[5,5,a]]).

tablero(1,6,6,6,[[0,0,x],[0,1,x],[0,2,x],[0,3,x],[0,4,x],[0,5,x],
[1,0,x],[1,1,a],[1,2,d],[1,3,x],[1,4,x],[1,5,x],
[2,0,x],[2,1,s],[2,2,x],[2,3,x],[2,4,x],[2,5,x],
[3,0,x],[3,1,x],[3,2,x],[3,3,x],[3,4,x],[3,5,x],
[4,0,x],[4,1,x],[4,2,d],[4,3,a],[4,4,s],[4,5,x],
[5,0,x],[5,1,x],[5,2,x],[5,3,x],[5,4,x],[5,5,x]]).

tablero(2,6,6,6,[[0,0,x],[0,1,x],[0,2,x],[0,3,x],[0,4,x],[0,5,s],
[1,0,x],[1,1,x],[1,2,x],[1,3,a],[1,4,d],[1,5,x],
[2,0,x],[2,1,x],[2,2,x],[2,3,x],[2,4,x],[2,5,x],
[3,0,x],[3,1,x],[3,2,x],[3,3,x],[3,4,x],[3,5,a],
[4,0,x],[4,1,x],[4,2,x],[4,3,x],[4,4,x],[4,5,x],
[5,0,s],[5,1,x],[5,2,x],[5,3,x],[5,4,x],[5,5,d]]).

tablero(0,10,10,10,[[0,0,s],[0,1,x],[0,2,x],[0,3,x],[0,4,x],[0,5,x],[0,6,x],[0,7,x],[0,8,x],[0,9,x],
[1,0,x],[1,1,x],[1,2,x],[1,3,s],[1,4,x],[1,5,x],[1,6,x],[1,7,x],[1,8,x],[1,9,x],
[2,0,x],[2,1,x],[2,2,a],[2,3,x],[2,4,x],[2,5,x],[2,6,x],[2,7,x],[2,8,x],[2,9,x],
[3,0,x],[3,1,x],[3,2,x],[3,3,x],[3,4,x],[3,5,x],[3,6,d],[3,7,x],[3,8,x],[3,9,x],
[4,0,x],[4,1,d],[4,2,x],[4,3,x],[4,4,x],[4,5,x],[4,6,x],[4,7,x],[4,8,x],[4,9,x],
[5,0,x],[5,1,x],[5,2,x],[5,3,x],[5,4,x],[5,5,x],[5,6,x],[5,7,x],[5,8,d],[5,9,x],
[6,0,x],[6,1,x],[6,2,x],[6,3,x],[6,4,x],[6,5,x],[6,6,x],[6,7,x],[6,8,d],[6,9,x],
[7,0,x],[7,1,x],[7,2,x],[7,3,x],[7,4,x],[7,5,x],[7,6,x],[7,7,x],[7,8,x],[7,9,x],
[8,0,x],[8,1,x],[8,2,x],[8,3,x],[8,4,x],[8,5,x],[8,6,x],[8,7,x],[8,8,x],[8,9,s],
[9,0,x],[9,1,x],[9,2,x],[9,3,x],[9,4,x],[9,5,x],[9,6,a],[9,7,x],[9,8,x],[9,9,a]]).

tablero(1,10,10,10,[[0,0,x],[0,1,x],[0,2,x],[0,3,x],[0,4,x],[0,5,x],[0,6,s],[0,7,a],[0,8,x],[0,9,x],
[1,0,x],[1,1,x],[1,2,x],[1,3,x],[1,4,x],[1,5,x],[1,6,x],[1,7,x],[1,8,x],[1,9,d],
[2,0,a],[2,1,x],[2,2,x],[2,3,x],[2,4,x],[2,5,x],[2,6,x],[2,7,x],[2,8,x],[2,9,d],
[3,0,x],[3,1,x],[3,2,x],[3,3,x],[3,4,x],[3,5,x],[3,6,x],[3,7,x],[3,8,x],[3,9,x],
[4,0,x],[4,1,x],[4,2,x],[4,3,x],[4,4,x],[4,5,x],[4,6,x],[4,7,x],[4,8,x],[4,9,x],
[5,0,x],[5,1,x],[5,2,x],[5,3,x],[5,4,x],[5,5,x],[5,6,x],[5,7,x],[5,8,x],[5,9,x],
[6,0,x],[6,1,x],[6,2,x],[6,3,x],[6,4,x],[6,5,x],[6,6,x],[6,7,x],[6,8,x],[6,9,x],
[7,0,d],[7,1,x],[7,2,x],[7,3,x],[7,4,x],[7,5,s],[7,6,x],[7,7,x],[7,8,x],[7,9,x],
[8,0,s],[8,1,x],[8,2,x],[8,3,x],[8,4,x],[8,5,x],[8,6,x],[8,7,x],[8,8,x],[8,9,s],
[9,0,a],[9,1,x],[9,2,x],[9,3,x],[9,4,x],[9,5,x],[9,6,x],[9,7,x],[9,8,x],[9,9,x]]).


tablero(_,20,20,20,[
[0,0,s],[0,1,s],[0,2,s],[0,3,x],[0,4,x],[0,5,x],[0,6,x],[0,7,x],[0,8,x],[0,9,x],[0,10,x],[0,11,x],[0,12,x],[0,13,x],[0,14,x],[0,15,x],[0,16,x],[0,17,x],[0,18,x],[0,19,x],
[1,0,x],[1,1,x],[1,2,x],[1,3,x],[1,4,x],[1,5,x],[1,6,x],[1,7,x],[1,8,x],[1,9,x],[1,10,a],[1,11,x],[1,12,x],[1,13,x],[1,14,x],[1,15,x],[1,16,x],[1,17,x],[1,18,a],[1,19,a],
[2,0,x],[2,1,x],[2,2,x],[2,3,x],[2,4,x],[2,5,x],[2,6,x],[2,7,x],[2,8,x],[2,9,d],[2,10,x],[2,11,x],[2,12,x],[2,13,x],[2,14,x],[2,15,x],[2,16,x],[2,17,x],[2,18,x],[2,19,d],
[3,0,x],[3,1,x],[3,2,x],[3,3,x],[3,4,x],[3,5,x],[3,6,x],[3,7,x],[3,8,x],[3,9,x],[3,10,x],[3,11,a],[3,12,x],[3,13,x],[3,14,x],[3,15,x],[3,16,x],[3,17,x],[3,18,x],[3,19,x],
[4,0,x],[4,1,x],[4,2,x],[4,3,x],[4,4,x],[4,5,x],[4,6,x],[4,7,x],[4,8,x],[4,9,x],[4,10,x],[4,11,x],[4,12,x],[4,13,x],[4,14,a],[4,15,x],[4,16,x],[4,17,x],[4,18,d],[4,19,x],
[5,0,x],[5,1,x],[5,2,x],[5,3,x],[5,4,x],[5,5,x],[5,6,x],[5,7,x],[5,8,x],[5,9,x],[5,10,x],[5,11,x],[5,12,x],[5,13,x],[5,14,x],[5,15,x],[5,16,x],[5,17,x],[5,18,x],[5,19,a],
[6,0,x],[6,1,x],[6,2,x],[6,3,x],[6,4,x],[6,5,x],[6,6,x],[6,7,x],[6,8,x],[6,9,x],[6,10,x],[6,11,x],[6,12,x],[6,13,x],[6,14,x],[6,15,x],[6,16,x],[6,17,x],[6,18,x],[6,19,x],
[7,0,x],[7,1,x],[7,2,x],[7,3,x],[7,4,x],[7,5,x],[7,6,x],[7,7,x],[7,8,x],[7,9,x],[7,10,x],[7,11,x],[7,12,x],[7,13,x],[7,14,x],[7,15,x],[7,16,x],[7,17,x],[7,18,x],[7,19,a],
[8,0,x],[8,1,x],[8,2,x],[8,3,x],[8,4,x],[8,5,x],[8,6,x],[8,7,x],[8,8,x],[8,9,x],[8,10,x],[8,11,x],[8,12,x],[8,13,x],[8,14,x],[8,15,x],[8,16,x],[8,17,x],[8,18,x],[8,19,d],
[9,0,x],[9,1,x],[9,2,x],[9,3,x],[9,4,x],[9,5,x],[9,6,x],[9,7,x],[9,8,x],[9,9,x],[9,10,x],[9,11,x],[9,12,x],[9,13,x],[9,14,x],[10,15,x],[9,16,x],[9,17,x],[9,18,x],[9,19,x],
[10,0,x],[10,1,x],[10,2,x],[10,3,x],[10,4,x],[10,5,x],[10,6,x],[10,7,x],[10,8,x],[10,9,x],[10,10,x],[10,11,x],[10,12,x],[10,13,x],[110,14,x],[10,15,x],[10,16,x],[10,17,x],[10,18,x],[10,19,x],
[11,0,x],[11,1,s],[11,2,x],[11,3,x],[11,4,x],[11,5,x],[11,6,x],[11,7,x],[11,8,x],[11,9,x],[11,10,x],[11,11,x],[11,12,x],[11,13,x],[11,14,x],[11,15,x],[11,16,x],[11,17,x],[11,18,x],[11,19,x],
[12,0,x],[12,1,s],[12,2,x],[12,3,x],[12,4,x],[12,5,x],[12,6,x],[12,7,x],[12,8,x],[12,9,x],[12,10,x],[12,11,x],[12,12,x],[12,13,x],[12,14,x],[12,15,x],[12,16,x],[12,17,x],[12,18,x],[12,19,x],
[13,0,x],[13,1,s],[13,2,x],[13,3,x],[13,4,x],[13,5,x],[13,6,x],[13,7,x],[13,8,x],[13,9,x],[13,10,x],[13,11,x],[13,12,x],[13,13,x],[13,14,x],[13,15,x],[13,16,x],[13,17,x],[13,18,x],[13,19,x],
[14,0,x],[14,1,x],[14,2,x],[14,3,x],[14,4,x],[14,5,x],[14,6,x],[14,7,x],[14,8,x],[14,9,x],[14,10,x],[14,11,x],[14,12,x],[14,13,x],[14,14,x],[14,15,x],[14,16,x],[14,17,x],[14,18,x],[14,19,x],
[15,0,x],[15,1,a],[15,2,x],[15,3,x],[15,4,x],[15,5,x],[15,6,x],[15,7,x],[15,8,x],[15,9,x],[15,10,x],[15,11,x],[15,12,x],[15,13,x],[15,14,x],[15,15,x],[15,16,x],[15,17,x],[15,18,x],[15,19,x],
[16,0,x],[16,1,x],[16,2,x],[16,3,x],[16,4,x],[16,5,x],[16,6,x],[16,7,x],[16,8,x],[16,9,x],[16,10,x],[16,11,x],[16,12,x],[16,13,x],[16,14,x],[16,15,x],[16,16,x],[16,17,x],[16,18,x],[16,19,x],
[17,0,x],[17,1,x],[17,2,x],[17,3,x],[17,4,x],[17,5,x],[17,6,x],[17,7,x],[17,8,x],[17,9,x],[17,10,a],[17,11,a],[17,12,a],[17,13,x],[17,14,x],[17,15,x],[17,16,x],[17,17,x],[18,18,x],[19,19,x],
[18,0,x],[18,1,x],[18,2,x],[18,3,x],[18,4,x],[18,5,x],[18,6,x],[18,7,x],[18,8,x],[18,9,x],[18,10,x],[18,11,x],[18,12,x],[18,13,x],[18,14,x],[18,15,x],[18,16,x],[18,17,x],[18,18,x],[18,19,x],[19,0,x],[19,1,x],[19,2,x],[19,3,x],[19,4,x],[19,5,x],[19,6,x],[19,7,x],[19,8,x],[19,9,x],[19,10,x],[19,11,x],[19,12,x],[19,13,x],[19,14,x],[19,15,x],[19,16,x],[19,17,x],[19,18,x],[19,19,x]
]).

% Predicado que permite usar un tablero definido en la base de
% conocimientos de la aplicacion.
createBoard(N,M,NumShips,BOARD):-
	X is random(3),tablero(X,N,M,NumShips,BOARD).
%Predicado que permite obtener la cabeza de una lista.
cabeza([H|_],H).
%Predicado que permite obtener la cola de una lista.
cola([_|T],T).
%Predicado que permite obtener los elementos de una sublista.
elementos([X,Y,Z|_],X,Y,Z).
%Predicado que permite salir de la recurision.
obtenerDimension([],Cnt,Cnt).
% Predicado que hace el llamado recursivo para calcular la cantidad de
% elementos en la lista.
obtenerDimension([_|T],Cnt,Res):-
	Acm is Cnt+1,obtenerDimension(T,Acm,Res).
% Predicado que usa el predicado obtenerDimension para obtener el valor
% de la dimension del tablero.
dimension(BOARD,Long):-
	obtenerDimension(BOARD,0,Long).
%Predicado que permite salir de la recursion obtenerFilas.
obtenerFilas([],Cnt,Cnt).
% Predicado que permite hacer el llamado recursivo para calcular la
% cantidad de filas en el tablero.
obtenerFilas([H|T],Cnt,Res):-
	elementos(H,_,Y,_),(Y=0->Acm is Cnt+1,obtenerFilas(T,Acm,Res);obtenerFilas(T,Cnt,Res)).
%Predicado que obtiene la cantidad de filas en el tablero.
filas(BOARD,Long):-
	obtenerFilas(BOARD,0,Long).
%Predicado que calcula la cantidad de columnas que hay en el tablero.
columnas(BOARD,C):-
	dimension(BOARD,L),filas(BOARD,F),C is L/F.
% Predicado que permite evaluar las coordenadas de la casilla dada.
coordenadas(X,Y,F,C):-
	integer(X),X>=0,X=<F,integer(Y),Y>=0,Y=<C.
%Predicado que permite evaluar el caracter en una casilla.
marObarco(S):-
	S=d;S=a;S=s;S=x;S=k.
% Predicado que permite saber si la cantidad de filas en el tablero es
% par.
filasPares(BOARD,Long):-
	filas(BOARD,Long),P is Long mod 2, P=0.
%Predicado que hace el llamado para evaluar una casilla del tablero.
revisarCasilla(BOARD,H,X,Y,Z,F,C):-
        cabeza(BOARD,H),elementos(H,X,Y,Z),coordenadas(X,Y,F,C),marObarco(Z).
%Predicado que permite salir del llamado recursivo de revisarMatriz.
revisarMatriz([],_,_).
% Predicado que hace el llamado recursivo para revisarMatriz. Su
% objetivo es darle una casilla a evaluar a la funcion revisarCasilla.
revisarMatriz(BOARD,F,C):-
	revisarCasilla(BOARD,_,_,_,_,F,C),cola(BOARD,T),revisarMatriz(T,F,C).
% Predicado que permite hacer las evaluaciones para saber si un tablero
% es valido. Para ello verifica que la cantidad de filas sea par, y que
% las casillas sean correspondientes a la representacion del tablero.
checkBoard(BOARD):-
	filasPares(BOARD,_),filas(BOARD,F),columnas(BOARD,C),revisarMatriz(BOARD,F,C).
%Predicado que permite obtener la coordenada x del disparo.
coordenadaXdisparo([H|_],H).
%Predicado que permite obtener la coordenada y del disparo.
coordenadaYdisparo([_,T],T).
%Predicado que permite salir de la recursion validarDisparo.
validarDisparo([],_).
% Predicado que permite iniciar la recursion validarDisparo. Esta
% funcion permite saber si las coordenadas a disparar son validas para
% el juego, esto es disparar a coordenadas validas y en la mitad del
% tablero enemigo(en N/2xM, de 0 hasta N/2).
validarDisparo(SHOOT,BOARD):-
    cabeza(SHOOT,S),coordenadaXdisparo(S,X),coordenadaYdisparo(S,Y),
    filas(BOARD,F),NF is F/2 - 1,columnas(BOARD,C),coordenadas(X,Y,NF,C),cola(SHOOT,T),validarDisparo(T,BOARD).
% Predicado que calcula la posicion en la lista en que se encuentra el
% usuario.
calcularPosicion(BOARD,L):-
	filas(BOARD,F),NF is F/2,columnas(BOARD,C),L is NF*C.
% Predicado que evalua si existe Ship determinado en tablero del
% usuario.
existe(Casilla,Ship):-
	elementos(Casilla,_,_,Z),Z=Ship.
%predicado que señala el fin del paso recursivo buscarShip.
buscarShip([],_,_,_,_):-
	false.
% Predicado recursivo que busca dentro de una posicion determinada si
% existe Ship en el tablero del usuario.
buscarShip([H|T],Cnt,Res,Pos,Ship):-
	Acm is Cnt + 1,Acm<Pos,buscarShip(T,Acm,Res,Pos,Ship);Acm is Cnt + 1,Acm>=Pos,existe(H,Ship);Acm is Cnt + 1,Acm >=Pos,buscarShip(T,Acm,Res,Pos,Ship).

% Predicado que evalua si existe el Ship determinado en el tablero del
% usuario.
validarShip(BOARD,Ship,Long):-
	calcularPosicion(BOARD,L),buscarShip(BOARD,0,Long,L,Ship).

play(BOARD,Ship,Position):-
	validarDisparo(Position,BOARD),validarShip(BOARD,Ship,_),display("Es posible realizar el disparo\n").


boardToString(Board,BoardStr):-
	cabeza(Board,H),elementos(H,_,_,Z),append(Z,"",BoardStr),display(BoardStr),cola(Board,T),boardToString(T,BoardStr).




















