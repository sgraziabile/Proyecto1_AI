:- module(path_finding,
	  [
	    buscar_plan_desplazamiento/4,
		raiz/1,
		padre/2,
		esMeta/1
	  ]).

:- use_module(module_beliefs_update, [node/5, at/3]).

:- dynamic padre/2, raiz/1, esMeta/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% eliminarPrimero(+Lista, +Elemento)
%
% Elimina el primer elemento de la lista.
%
eliminarPrimero([], []).
eliminarPrimero([_|Xs], Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% seleccionar(+Nodo, +Frontera, +FronteraSinNodo)
%	
% Selecciona el primer nodo de la lista Frontera.
%	
seleccionar(Nodo, [Nodo|RestoLista], RestoLista).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% encontrarCamino(+Meta, -Camino)
%
% Encuentra un camino a un nodo Meta.
% Usa las relaciones padre(Hijo, Padre) que se van agregando a la base de conocimiento
% cuando se agregan nuevos vecinos a la nueva frontera, 
% en la busqueda de llegar de un nodo origen a uno destino.
%
encontrarCamino(Nodo, []):- raiz(Nodo), !.
encontrarCamino(Nodo, [P|Camino]):-
	padre(Nodo, P),
	encontrarCamino(P, Camino).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
%
% crearPlan(+Camino, -Plan)
%
% Crea plan de movimientos para un camino elegido.
% Para cada nodo de un camino, crea una lista de acciones de movimiento avanzar(IdNodo)
% donde IdNodo es un identificador de un nodo.
% Camino es una lista conteniendo identificadores de nodos.
%
crearPlan([], []).
crearPlan(Camino, Plan):-
	findall(avanzar(Nodo), member(Nodo, Camino), Plan).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% generarVecinos(+Nodo,-Vecinos)
%	Encuentra todos los nodos del árbol de búsqueda que
% representan estados alcanzables desde el estado de Nodo
% aplicando alguno de los operadores disponibles. 
%
generarVecinos([IdNodo,_], Vecinos):-
	node(IdNodo, _, _, _, Conexiones),
	findall(
		[IdVecino, CostoVecino],
		(
			member([IdVecino, CostoVecino], Conexiones)
		), 
		Vecinos 
	).	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% compararH(+Nodo1, +Nodo2, ?Resultado)
%
% Compara dos nodos por el valor de la heurística.
%
compararH([_,_,H1], [_,_,H2], Result):- 
	H1 < H2, Result is -1.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%ordenarPorH(+Lista, -ListaOrdenada)
%
% Ordena la lista de nodos de menor a mayor por el valor de la heurística.
%
ordenarPorH([], []).
ordenarPorH(VecinosConH, VecinosOrdenados):-
	predsort(compararH, VecinosConH, VecinosOrdenados).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% agregar(+Frontera, +Vecinos, -NuevaFrontera, +NuevosVisitados, +Padre, +Metas)
% Agrega los vecinos de Padre a la frontera, siguiendo el algoritmo A*
%
agregar(Frontera, [], Frontera,_, _, _):- !.
agregar(Frontera, Vecinos, NuevaFrontera, Visitados, Padre, Metas):-
	Padre = [IdPadre, CostoPadre],
	calcular_menor_H(IdPadre, Metas, HPadre),
	findall(
		[Hijo, F],
		(
			member([Hijo, CostoHijo],Vecinos),            %para cada vecino
			( %Caso 1: si el hijo esta en la frontera
				member([Hijo,ViejoF], Frontera),
				\+ member([Hijo,_], Visitados) ->
				( 
					G is (CostoPadre - HPadre) + CostoHijo,      %g(n) = costo acumulado 
					calcular_menor_H(Hijo, Metas, HMeta),        %calculo el menor H de los hijos
					FTemp is G + HMeta,                 %f(n) = g(n) + h(n) 
					FTemp < ViejoF -> (
						F = FTemp,
						retractall(padre(Hijo, _)),               %elimino la relacion padre(Hijo, _)
						assert(padre(Hijo, IdPadre))                %agrego la relacion padre(Hijo, Padre)
					); false                                   %si no, lo mantengo igual
				)                              
			; %Caso 2: si el hijo no esta en la frontera
					\+ member([Hijo,_],Frontera), 
					\+ member([Hijo,_], Visitados) ->             %si el hijo no esta en visitados
					(
						G is (CostoPadre - HPadre) + CostoHijo,      %g(n) = costo acumulado  ES NECESARIO VOLVER A SUMAR?
						calcular_menor_H(Hijo, Metas, HMeta),    %calculo el menor H de los hijos
						F is G + HMeta,                      %f(n) = g(n) + h(n)
						retractall(padre(Hijo, _)),               %elimino la relacion padre(Hijo, _)
						assert(padre(Hijo, IdPadre))                %agrego la relacion padre(Hijo, Padre)
					)
					; 
					\+ member([Hijo,_],Frontera),
					member([Hijo,ViejoF], Visitados) -> (               %si el hijo esta en visitados
						G is (CostoPadre - HPadre) + CostoHijo,      %g(n) = costo acumulado 
						calcular_menor_H(Hijo, Metas, HMeta),        %calculo el menor H de los hijos
						FTemp is G + HMeta,                 %f(n) = g(n) + h(n) 
						FTemp < ViejoF -> (
							F = FTemp, 
							delete(Visitados, [Hijo, _],VisitadosAux),    %elimino el hijo de visitados
							retractall(padre(Hijo, _)),               %elimino la relacion padre(Hijo, _)
							assert(padre(Hijo, IdPadre))                %agrego la relacion padre(Hijo, Padre)
						); false
					)
				)
		),
		VecinosSinRep
	),
	findall(
		[Nodo,Costo],
		(
			member([Nodo,Costo],Frontera),
			\+ member([Nodo,_],VecinosSinRep)
		),
		FronteraSinEliminados
	),
	append(VecinosSinRep, FronteraSinEliminados, FronteraFinal),
	ordenar_segun_f(FronteraFinal, NuevaFrontera).

  ordenar_segun_f(Frontera, NuevaFrontera):-
    sort(2, @=<, Frontera, NuevaFrontera).    %ordenar Frontera por el segundo elemento. 
	
	calcular_menor_H(Nodo, Metas, MenorH):-
		findall(
			H,
			(
				member(Meta,Metas),
				calcularH(Nodo, Meta, H)    %h(n) = heuristica
			), 
				ListaHeuristicas
			),
			min_list(ListaHeuristicas, MenorH).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscar_plan_desplazamiento(+Metas, -Plan, -Destino, -Costo)
% Agregar todas las metas como hechos esMeta(idNodoMeta)
% Si tiene al menos una meta, pone el nodo actual del agente como raiz del árbol de búsqueda
% y busca el camino desde la posición del agente a un meta
% usando A* (buscarEstrella/5)
%

buscar_plan_desplazamiento(Metas, Plan, Destino, Costo):-
	forall(member(Meta, Metas), assert(esMeta(Meta))),
	at(MyNode, agente, me),
	length(Metas, CantMetas),
	CantMetas > 0,
	!,
	retractall(raiz(_)),
	assert(raiz(MyNode)),
	buscarEstrella([[MyNode, 0]], Metas, Camino, Costo, Destino),
	crearPlan(Camino, Plan).
	
buscar_plan_desplazamiento(_, [], [], 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscarEstrella(+Frontera, +Metas, ?Camino, ?Costo, ?Destino)
% 
% Busca el camino optimo desde la frontera hacia la meta mas cercana, utilizando la estrategia de busqueda A*.
%
	
buscarEstrella(Frontera, Metas, C3, Costo, Destino):-
	buscar(Frontera, [], Metas, Destino),
	encontrarCamino(Destino, C),
	append([Destino], C, C2),	
	reverse(C2, C3),
	costoCamino(C3, Costo),
	eliminarPrimero(C3, Camino),
	retractall(esMeta(_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscar(+Frontera, +Visitados, +Metas, -Destino)
% 
% Busca el camino optimo desde la frontera hacia la Meta, utilizando la estrategia de busqueda A*.
% No devuelve el camino como un parametro, sino que agrega las relaciones padre(Hijo, Padre)
% que permita luego encontrar el camino y su costo.
%
% Caso 1: Si el nodo es meta, termina la búsqueda.
% Caso 2: Si el nodo no es meta
% Selecciono el primer nodo de la frontera, 
% Genera los vecinos,
% Agregar nodo a visitados,
% Agregar vecinos a frontera, con los cuidados necesarios de A*
% y llama recursivmaente con la nueva frontera.
	
buscar(Frontera, _, _M, Nodo):-
	seleccionar([Nodo, _], Frontera, _),
	esMeta(Nodo),
	!.

buscar(Frontera, Visitados, Metas, MM):-
	seleccionar(Nodo, Frontera, FronteraSinNodo), % selecciona primer nodo de la frontera
	generarVecinos(Nodo, Vecinos), % genera los vecinos del nodo - TO-DO
	agregarAVisitados(Nodo, Visitados, NuevosVisitados), % agrega el nodo a lista de visitados
	agregar(FronteraSinNodo, Vecinos, NuevaFrontera, NuevosVisitados, Nodo, Metas), % agrega vecinos a la frontera - TO-DO
	buscar(NuevaFrontera, NuevosVisitados, Metas, MM). % continua la busqueda con la nueva frontera

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%
% agregarAVisitados(+Nodo, +Visitados, ?VisitadosConNodo)
%
% Agrega un nodo a la lista de visitados.
%
agregarAVisitados(Nodo, Visitados, [Nodo | Visitados]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% costoCamino(+Lista, ?Costo)
%
% Calcula el costo del camino, 
% como la sumatoria de los costos de los nodos que forma el camino.
% Lista es una lista conteniendo identificadores de nodos, representando el camino.
%
costoCamino([], 0).

costoCamino([X|Xs], R):-
	node(X, _, _, CostoNodo, _),
	costoCamino(Xs, CostoResto),
	R is CostoNodo + CostoResto.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% calcularH(+Nodo, ?Resultado, +Meta)
%
% Calcula el valor de la heurística para el nodo Nodo a una Meta.
% La heurística es la distancia euclidea.
%
calcularH(Nodo, Meta, Resultado):-
	node(Meta, X2, Y2, _, _),
	node(Nodo, X1, Y1, _, _),
	distance([X1, Y1], [X2, Y2], Resultado).

distance([X1, Y1], [X2, Y2], Distance):-
	DX is X2 - X1,
	DY is Y2 - Y1,
	Distance is sqrt(DX^2 + DY^2).

print_list([]).
print_list([X|Xs]):-
	write(X), nl,
	print_list(Xs).
