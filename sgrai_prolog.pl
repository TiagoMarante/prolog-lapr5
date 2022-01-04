listaadjacencias(No,Nivel,Result):-
    lista_adjacencias_por_nivel(No,0,Nivel,[]),
    findall([[[Amigo]],Forca], teste(Amigo,Forca), Result),
    retractall(teste(_,_)).

lista_adjacencias_por_nivel(No,Nivel_Atual,Nivel,Visitado):-
    no(ID,No,_),
    findall(
        [Amigo,Forca],
        ((ligacao(ID,IDAmigo,Forca,_);ligacao(IDAmigo,ID,_,Forca)),no(IDAmigo,Amigo,_)),
        Amigos),
    assert(teste(No,Amigos)),    
    append([No], Visitado, Visitado1),
    % write(Nivel_Atual),nl,write(No),nl,write(Amigos),nl,nl,
    Nivel_Atual1 is Nivel_Atual + 1,
    !,
    outroPredicado(Amigos, Nivel_Atual1,Nivel,Visitado1).

outroPredicado([],_,_,_):-!.
outroPredicado([[H|_]|T], Nivel_Atual,Nivel,Visitado):-
    \+ member(H,Visitado),
    Nivel_Atual =< Nivel,
    lista_adjacencias_por_nivel(H,Nivel_Atual,Nivel,Visitado),
    !,
    outroPredicado(T, Nivel_Atual, Nivel,Visitado).
outroPredicado([[_|_]|T], Nivel_Atual,Nivel,Visitado):-
    !,
    outroPredicado(T, Nivel_Atual, Nivel,Visitado).