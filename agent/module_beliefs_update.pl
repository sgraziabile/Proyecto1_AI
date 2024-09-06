:- module(beliefs_update,
	  [
	    update_beliefs/1,
	    time/1,
	    node/5,
	    at/3,
		direction/1
	  ]).

:- dynamic time/1, node/5, at/3, direction/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TO-DO
%
% update_beliefs(+Perc)
%
% IMPORTANTE: Debe exportarse todo predicado dinámico (creencia)
% manipulado por la actualización de creencias, para que puedan ser
% consultado por el resto del código del agente.
%
% El parámetro Perc recibe una lista con el siguiente formato: [N1,...,Nk,A1,...Ap,Time,Dir]
% donde: 
% N1 a Nk son k elementos (k>0) de la forma node(Id, PosX, PosY, Costo, Conexiones),
% A1 a Ap son p elementos (p>0) de la forma at(IdNodo, TipoEntidad, IdEntidad),
% Time es el functor time(T), donde T es el tiempo actual (descendente) de la partida.
% Dir es el functor direction(D), donde D ∈ {w, s, a, d}.
%
% Este agente básico, al recibir una nueva percepcion olvida todo lo que tenía guardado en su estado interno
% Y almance la información de la percepción nueva.
%
% Pueden realizar todos los cambios de implementación que consideren necesarios.
% Esta implementación busca ser un marco para facilitar la resolución del proyecto.

update_beliefs(Perc):-
%Procesar los nodos que detecta (node)
forall(
	member(node(Id,PosX,PosY,Costo,Conexiones),Perc),
	(
		retractall(node(Id,_,_,_,_)), %elimino información desactualizada del nodo
		asserta(node(Id,PosX,PosY,Costo,Conexiones))	%agrego la infomación actualizada del nodo
	)
),
%Procesar las posiciones de las entidades (at)
forall(
	member(at(IdNodo,TipoEntidad,IdEntidad),Perc),
	(
		retractall(at(IdNodo,_,_)),
		retractall(at(_,agente,me)),
		asserta(at(IdNodo,TipoEntidad,IdEntidad))
	)
),
%Procesar tiempo
(
	member(time(T),Perc) 
	-> retractall(time(_)), asserta(time(T))
	; true
),
%Procesar direccion
(
	member(direction(D),Perc)
	-> retractall(direction(_)), asserta(direction(D))
	; true
).