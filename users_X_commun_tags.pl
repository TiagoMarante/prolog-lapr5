users_X_commun_tags(X,List_Result):-
    get_time(T1),
    obter_todas_tags(Todas_Tags),
    findall(Combinacao,combinations(X,Todas_Tags,Combinacao),Combinacoes),
    findall(User,no(_,User,_),Users),
    users_tags_comuns_combinacao(X,Users,Combinacoes),
    findall([Comb,ListUsers],users_combinacao(Comb,ListUsers),List_Result),
    retractall(users_combinacao(_,_)),
    write('Solucao encontrada em '),
    get_time(T2),
    T is T2-T1,write(T),write(' segundos'),nl.

%=== obter users com tags comuns por cada combinaçao ===
users_tags_comuns_combinacao(_,_,[]).
users_tags_comuns_combinacao(X,Users,[Combinacao|Combinacoes]):-
    verificar_sinonimos(Combinacao,Combinacao_Sinonimos),
    users_X_commun_tags2(X,Combinacao_Sinonimos,Users,Users_Com_Tags),
    users_tags_comuns_combinacao(X,Users,Combinacoes),
    !,
    assertz(users_combinacao(Combinacao,Users_Com_Tags)).

%=== obter lista de users com tags em comum da combinação ===
users_X_commun_tags2(_,_,[],[]):-!.
users_X_commun_tags2(X,Tags,[U|Users],Result):-
    no(_,U,User_Tags),
    intersection(Tags, User_Tags,Commun),
    length(Commun, Size),
    Size >= X, !,
    users_X_commun_tags2(X,Tags,Users,Result1),
    append([U], Result1, Result).
users_X_commun_tags2(X,Tags,[_|Users],Result):-
    !,
    users_X_commun_tags2(X,Tags,Users,Result).

%=== Obter todas as tags dos users ===
obter_todas_tags(Tags):-
    findall(User_Tags,no(_,_,User_Tags),Todas_Tags),
    remover_tags_repetidas(Todas_Tags,Tags).

remover_tags_repetidas([],[]).
remover_tags_repetidas([Lista|Todas_Tags],Tags):-
    remover_tags_repetidas(Todas_Tags,Tags1),!,
    union(Lista,Tags1,Tags).

%=== Verificar as tags com sinonimos ===
verificar_sinonimos([],[]):-!.
verificar_sinonimos([Tag|List_Tags],List_Sinonimos):-
    (sinonimos(Tag,Sin);sinonimos(Sin,Tag)),
    verificar_sinonimos(List_Tags,List_Sinonimos1),!,
    append([Tag,Sin], List_Sinonimos1, List_Sinonimos).
verificar_sinonimos([Tag|List_Tags],List_Sinonimos):-
    verificar_sinonimos(List_Tags,List_Sinonimos1),!,
    append([Tag], List_Sinonimos1, List_Sinonimos).

%=== Cominaçoes ===
combinations(0,_,[]).
combinations(N,[X|T],[X|Comb]):-N>0,N1 is N-1,combinations(N1,T,Comb).
combinations(N,[_|T],Comb):-N>0,combinations(N,T,Comb).