


%node(a,1,1,1,[[b,1],[d,1],[c,1],[e,1]]).
%node(b,1,0,1,[[a,1]]).
%node(d,0,1,1,[[a,1]]).
%node(c,2,1,1,[[a,1]]).
%node(e,1,2,1,[[a,1]]).

node(a,0,0,1,[[b,1],[d,20],[e,1]]).
node(b,1,0,1,[[a,1],[d,20],[e,1],[f,1],[g,20]]).
node(c,2,0,50,[[b,1],[e,1],[f,1],[k,20],[j,1]]).
node(j,3,0,50,[[c,20],[f,1],[k,20]]).
node(d,0,1,1,[[a,1],[b,1],[e,1]]).
node(e,1,1,1,[[a,1],[b,1],[c,20],[d,20],[f,1],[g,20],[h,1],[i,1]]).
node(f,2,1,1,[[b,1],[c,20],[j,1],[e,1],[k,20],[h,1],[i,1],[l,1]]).
node(k,3,1,50,[[c,20],[j,1],[f,1],[i,1],[c,20]]).
node(g,0,2,50,[[20,1],[e,1],[h,1]]).
node(h,1,2,1,[[d,20],[e,1],[g,20],[i,1]]).
node(i,2,2,1,[[e,1],[f,1],[k,20],[h,1],[c,20]]).
node(l,3,2,1,[[f,1],[k,20],[i,1]]).


generarVecinos([IdNodo, CostoActual], Vecinos):-
	node(IdNodo, _, _, _, Conexiones),
	findall(
		[IdVecino, CostoTotal],
		(
			member([IdVecino, CostoVecino], Conexiones),
			CostoTotal is CostoActual + CostoVecino
		), 
		Vecinos 
	).
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

  eliminar(_,[],[]).
  eliminar([X|Xs],X,Xs).
  eliminar([Y|Ys],X,[Y|Zs]):-eliminar(Ys,X,Zs).

  print_list([]).
  print_list([X|Xs]):-
    write(X), nl,
    print_list(Xs).

  agregar(Frontera, Vecinos, NuevaFrontera,Visitados, Padre, Metas):-
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
            writeln('Front, NoVisitados'),
            G is (CostoPadre - HPadre) + CostoHijo,      %g(n) = costo acumulado 
            calcular_menor_H(Hijo, Metas, HMeta),        %calculo el menor H de los hijos
            FTemp is G + HMeta + HPadre,                 %f(n) = g(n) + h(n) 
            FTemp < ViejoF -> (
              F = FTemp
              %delete(Frontera,[Hijo,ViejoF],FronteraAux)
            ); false                                   %si no, lo mantengo igual
          )                              
        ; %Caso 2: si el hijo no esta en la frontera
            \+ member([Hijo,_],Frontera), 
            \+ member([Hijo,_], Visitados) ->             %si el hijo no esta en visitados
            (
              writeln('NoFront, NoVisitados'),
              G is (CostoPadre - HPadre) + CostoHijo,      %g(n) = costo acumulado  ES NECESARIO VOLVER A SUMAR?
              calcular_menor_H(Hijo, Metas, HMeta),    %calculo el menor H de los hijos
              F is G + HMeta + HPadre                       %f(n) = g(n) + h(n)    
            )
            ; 
            \+ member([Hijo,_],Frontera),
            member([Hijo,ViejoF], Visitados) -> (               %si el hijo esta en visitados
              writeln('NoFront, Visitados'),
              G is (CostoPadre - HPadre) + CostoHijo,      %g(n) = costo acumulado 
              calcular_menor_H(Hijo, Metas, HMeta),        %calculo el menor H de los hijos
              FTemp is G + HMeta + HPadre,                 %f(n) = g(n) + h(n) 
              FTemp < ViejoF -> (
                F = FTemp 
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

  calcularH(Nodo, Meta, Resultado):-
    node(Meta, X2, Y2, _, _),
    node(Nodo, X1, Y1, _, _),
    distance([X1, Y1], [X2, Y2], Resultado).

    distance([X1, Y1], [X2, Y2], Distance):-
      DX is X2 - X1,
      DY is Y2 - Y1,
      Distance is sqrt(DX^2 + DY^2).

		