:-dynamic melhor_sol_minlig/2.

all_dfs_fake(Nome1,Nome2,LCam):-get_time(T1),
    findall(Cam,dfs(Nome1,Nome2,Cam),LCam),
    length(LCam,NLCam),
    get_time(T2),
    write(NLCam),write(' solucoes encontradas em '),
    T is T2-T1,write(T),write(' segundos'),nl,
    write('Lista de Caminhos possiveis: '),write(LCam),nl,nl.

dfs(Orig,Dest,Cam):-dfs2(Orig,Dest,[Orig],Cam).

dfs2(Dest,Dest,LA,Cam):-!,reverse(LA,Cam).
dfs2(Act,Dest,LA,Cam):-no(NAct,Act,_),(ligacao(NAct,NX,_,_);ligacao(NX,NAct,_,_)),
    no(NX,X,_),\+ member(X,LA),dfs2(X,Dest,[X|LA],Cam).


plan_minlig(Orig,Dest,LCaminho_minlig):-
		get_time(Ti),
		(melhor_caminho_minlig(Orig,Dest);true),
		retract(melhor_sol_minlig(LCaminho_minlig,_)),
		get_time(Tf),
		T is Tf-Ti,
		write('Tempo de geracao da solucao:'),write(T),nl.

melhor_caminho_minlig(Orig,Dest):-
		asserta(melhor_sol_minlig(_,10000)),
		dfs(Orig,Dest,LCaminho),
		atualiza_melhor_minlig(LCaminho),
		fail.

atualiza_melhor_minlig(LCaminho):-
		melhor_sol_minlig(_,N),
		length(LCaminho,C),
		C<N,retract(melhor_sol_minlig(_,_)),
		asserta(melhor_sol_minlig(LCaminho,C)).


% ----------------------- Caminho mais Curto ----------------------------

dfsMin(Orig,Dest,Cam):-dfsMin2(Orig,Dest,[Orig],Cam).

dfsMin2(Dest,Dest,LA,Cam):-!,reverse(LA,Cam).
dfsMin2(Act,Dest,LA,Cam):-
		melhor_sol_minlig(_,N,1), length([_|LA],C), C<N,!,
		no(NAct,Act,_), (ligacao(NAct,NX,_,_);ligacao(NX,NAct,_,_)), no(NX,X,_),\+ member(X,LA),dfsMin2(X,Dest,[X|LA],Cam).
dfsMin2(Act,Dest,LA,Cam):-
		melhor_sol_minlig(_,_,0),
		no(NAct,Act,_), (ligacao(NAct,NX,_,_);ligacao(NX,NAct,_,_)), no(NX,X,_),\+ member(X,LA),dfsMin2(X,Dest,[X|LA],Cam).		

:-dynamic melhor_sol_minlig/3.

caminho_mais_curto(Orig,Dest,LCaminho_minlig):-
		(melhor_caminho_curto(Orig,Dest);true),
		retract(melhor_sol_minlig(LCaminho_minlig,_,_)).

melhor_caminho_curto(Orig,Dest):-
		asserta(melhor_sol_minlig(_,_,0)),
		dfsMin(Orig,Dest,LCaminho),
		atualiza_mais_curto(LCaminho),
		fail.

atualiza_mais_curto(LCaminho):-
		length(LCaminho,C),
		((melhor_sol_minlig(_,_,0),!,retract(melhor_sol_minlig(_,_,_)),asserta(melhor_sol_minlig(LCaminho,C,1)))
		;(melhor_sol_minlig(_,N,_),C<N,retract(melhor_sol_minlig(_,_,_)), asserta(melhor_sol_minlig(LCaminho,C,1)))).

% ----------------------- Caminho mais Curto ----------------------------

% ----------------------- Tamanho de Rede -------------------------------
list_length([]     , 0 ).
list_length([_|Xs] , L ) :- list_length(Xs,N) , L is N+1 .


tamanhoRede1(_,0,[]) :- !.
tamanhoRede1(Orig, Counter, LIST):- 
    no(NUM,Orig,_),
	(ligacao(NUM,New_User_Num,_,_);ligacao(New_User_Num,NUM,_,_)),
    no(New_User_Num,USER,_),
    Counter1 is Counter-1, Counter1 >= 0,
    tamanhoRede1(USER,Counter1, LIST1),
    append([USER], LIST1, LIST).


tamanhoRede(Orig, Counter, LIST):- 
    findall(LIST1, tamanhoRede1(Orig, Counter, LIST1), NEWL), 
    flatten(NEWL, LIST1),
    sort(LIST1, LIST).

% ----------------------- Tamanho de Rede -------------------------------




% ----------------------- Sugerir Utilizadores com base em tags, ligações e por nivel -------------------------------


%tags do user -> Usar nome
tags_user(User, Lista):- no(_,User,Lista).

%intersection de duas listas na lista L3%
intersection([], _, []).
intersection([Head|L1tail], L2, L3) :-
        memberchk(Head, L2),
        !,
        L3 = [Head|L3tail],
        intersection(L1tail, L2, L3tail).
intersection([_|L1tail], L2, L3) :-
        intersection(L1tail, L2, L3).

% Orig -> user a que queremos sugerir amigos
% N -> nivel ate que queremos ir
% Resultado -> Lista com users que podemos sugerir

extract_Not_Friends(_,[],[]).
extract_Not_Friends(Orig,[P|ListaTotal], Resultado):- no(ID,Orig,_),no(IDP,P,_),
                                                    \+(ligacao(ID,IDP,_,_);ligacao(IDP,ID,_,_)),
                                                    !,
                                                    extract_Not_Friends(Orig,ListaTotal, Resultado1),
    												append([P],Resultado1, Resultado).  

extract_Not_Friends(Orig,[_|ListaTotal], Resultado):-extract_Not_Friends(Orig,ListaTotal, Resultado).

tags_comum(_,[],[]).
tags_comum(Orig, [ID|ListaFinal], Resultado):- tags_user(Orig, ListaA), tags_user(ID, ListaB),
                                                intersection(ListaA, ListaB, Result),
                                                length(Result, Tamanho),
                                                Tamanho >= 1,
                                                !,
                                                tags_comum(Orig,ListaFinal, Resultado1),
    											append([ID],Resultado1, Resultado).

tags_comum(Orig, [_|ListaFinal], Resultado) :- tags_comum(Orig, ListaFinal, Resultado).

sugerir_conexao(Orig, N, Resultado):-   tamanhoRede(Orig,N,ListNivel), 
    									sort(ListNivel, ListNivelF),
                                        extract_Not_Friends(Orig,ListNivelF, ListFinal),
                                        tags_comum(Orig,ListFinal, Resultado).	 
										

% ----------------------- Sugerir Utilizadores com base em tags, ligações e por nivel -------------------------------


% ----------------------- Tags em comum -------------------------------

sinonimos('c#','csharp').
sinonimos(musica,muzica).

% ----------------------- Tags em comum -------------------------------


% -------------------- Caminho mais seguro ----------------------------

dfsSeguro(Orig,Dest,FMin,Cam,Sum):-dfs2Seguro(Orig,Dest,[Orig],Cam,FMin,0,Sum).

dfs2Seguro(Dest,Dest,LA,Cam,_,Sum,Sum):-!,reverse(LA,Cam).
dfs2Seguro(Act,Dest,LA,Cam,FMin,SAux,Sum):-no(NAct,Act,_), 
										   (ligacao(NAct,NX,F,_);ligacao(NX,NAct,F,_)), F >= FMin, SAux1 is SAux + F,
										   no(NX,X,_),\+ member(X,LA),dfs2Seguro(X,Dest,[X|LA],Cam,FMin,SAux1,Sum).


:-dynamic melhor_sol_seguro/3.

caminho_mais_seguro(Orig,Dest,FMin,LCaminho_seguro):-
		(melhor_caminho_seguro(Orig,Dest,FMin);true),
		retract(melhor_sol_seguro(LCaminho_seguro,_,_)).

melhor_caminho_seguro(Orig,Dest,FMin):-
		asserta(melhor_sol_seguro(_,_,0)),
		dfsSeguro(Orig,Dest,FMin,LCaminho,Sum),
		atualiza_mais_seguro(LCaminho,Sum),
		fail.

atualiza_mais_seguro(LCaminho,Sum):-
		((melhor_sol_seguro(_,_,0),!,retract(melhor_sol_seguro(_,_,_)),asserta(melhor_sol_seguro(LCaminho,Sum,1)))
		;(melhor_sol_seguro(_,SumMax,_), Sum>SumMax,retract(melhor_sol_seguro(_,_,_)), asserta(melhor_sol_seguro(LCaminho,Sum,1)))).

% -------------------- Caminho mais seguro ----------------------------		
% -------------------- Caminho mais forte ----------------------------		

:-dynamic melhor_sol_forte/2.

all_dfs(Nome1,Nome2,LCam):-
    get_time(T1),
    findall((F,Cam),dfs(Nome1,Nome2,Cam,F),LCam),
    length(LCam,NLCam),
    get_time(T2),
    nl, write(NLCam),write(' solucoes encontradas em '),
    T is T2-T1,write(T),write(' segundos'),nl,
    write('Lista de Caminhos possiveis: '),write(NLCam).

dfs(Orig,Dest,Cam,Counter):-
    dfs2(Orig,Dest,[Orig],Cam,Counter).

dfs2(Dest,Dest,LA,Cam,0):-!,
    reverse(LA,Cam).
dfs2(Act,Dest,LA,Cam,Counter):-
    %%no(NAct,Act,_),ligacao(NAct,NX,A,_),no(NX,X,_),\+ member(X,LA), %% unidirecional no sentido da travessia
    %%no(NAct,Act,_),ligacao(NAct,NX,A,_),no(NX,X,_),\+ member(X,LA), %% unidirecional nos dois sentidos
    no(NAct,Act,_),(ligacao(NAct,NX,A,_);ligacao(NX,NAct,A,_)),no(NX,X,_),\+ member(X,LA), %%  bidirecional  no sentido da travessia
    %% no(NAct,Act,_),(ligacao(NAct,NX,A,B);ligacao(NX,NAct,A,B)),no(NX,X,_),\+ member(X,LA), %%  bidirecional nos dois sentidos
    dfs2(X,Dest,[X|LA],Cam,Counter1),
    Counter is Counter1 +A . %% no sentido da travessia
  %%Counter is Counter1 + A +B. nos dois sentidos

plan_maisforte(Orig,Dest,LCaminho_minlig,Counter):-
    	
		(melhor_caminho_forte(Orig,Dest,Counter);true),
		retract(melhor_sol_forte(LCaminho_minlig,Counter)).

melhor_caminho_forte(Orig,Dest,Counter):-
        
		asserta(melhor_sol_forte(_,-10000)),
		dfs(Orig,Dest,LCaminho,Counter),
		atualiza_caminho_mais_forte(LCaminho,Counter),
		fail.

atualiza_caminho_mais_forte(LCaminho,Counter):-
		melhor_sol_forte(_,N),
		Counter>N,retract(melhor_sol_forte(_,_)),
		asserta(melhor_sol_forte(LCaminho,Counter)).

% -------------------- Caminho mais forte ----------------------------	