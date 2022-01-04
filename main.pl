:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

:- http_handler(/, root_handler, []).
:- http_handler('/db', db, []).

root_handler(_):-
        format('Content-Type: text/html~n~n', []),
        findall(Fact, some_fact(Fact), Facts),
        atomic_list_concat(Facts, ', ', Atom),
        format('Hello from Prolog: ~w', [Atom]).

db(_):-
        heroku_db_connect(Connection),
        format('Content-Type: text/html~n~n', []),        
        format('We have a connection: ~q', [Connection]).


% Base de Conhecimento

% no(1,ana,[natureza,pintura,musica,sw,porto]).
% no(11,antonio,[natureza,pintura,carros,futebol,lisboa]).
% no(12,beatriz,[natureza,musica,carros,porto,moda]).
% no(13,carlos,[natureza,musica,sw,futebol,coimbra]).
% no(14,daniel,[natureza,cinema,jogos,sw,moda]).
% no(21,eduardo,[natureza,cinema,teatro,carros,coimbra]).
% no(22,isabel,[natureza,musica,porto,lisboa,cinema]).
% no(23,jose,[natureza,pintura,sw,musica,carros,lisboa]).%
% no(24,luisa,[natureza,cinema,jogos,moda,porto]).
% no(31,maria,[natureza,pintura,musica,moda,porto]).%
% no(32,anabela,[natureza,cinema,musica,tecnologia,porto]).
% no(33,andre,[natureza,carros,futebol,coimbra]).
% no(34,catia,[natureza,musica,cinema,lisboa,moda]).
% no(41,cesar,[natureza,teatro,tecnologia,futebol,porto]).
% no(42,diogo,[natureza,futebol,sw,jogos,porto]).
% no(43,ernesto,[natureza,teatro,carros,porto]).
% no(44,isaura,[natureza,moda,tecnologia,cinema]).
% no(200,sara,[natureza,moda,musica,sw,coimbra]).

% no(51,rodolfo,[natureza,musica,sw]).
% no(61,rita,[moda,tecnologia,cinema]).


% ligacao(1,11,10,8).
% ligacao(1,12,2,6).
% ligacao(1,13,-3,-2).
% ligacao(1,14,1,-5).
% ligacao(11,21,5,7).
% ligacao(11,22,2,-4).
% ligacao(11,23,-2,8).
% ligacao(11,24,6,0).
% ligacao(12,21,4,9).
% ligacao(12,22,-3,-8).
% ligacao(12,23,2,4).
% ligacao(12,24,-2,4).
% ligacao(13,21,3,2).
% ligacao(13,22,0,-3).
% ligacao(13,23,5,9).
% ligacao(13,24,-2, 4).
% ligacao(14,21,2,6).
% ligacao(14,22,6,-3).
% ligacao(14,23,7,0).
% ligacao(14,24,2,2).
% ligacao(21,31,2,1).
% ligacao(21,32,-2,3).
% ligacao(21,33,3,5).
% ligacao(21,34,4,2).
% ligacao(22,31,5,-4).
% ligacao(22,32,-1,6).
% ligacao(22,33,2,1).
% ligacao(22,34,2,3).
% ligacao(23,31,4,-3).
% ligacao(23,32,3,5).
% ligacao(23,33,4,1).
% ligacao(23,34,-2,-3).
% ligacao(24,31,1,-5).
% ligacao(24,32,1,0).
% ligacao(24,33,3,-1).
% ligacao(24,34,-1,5).
% ligacao(31,41,2,4).
% ligacao(31,42,6,3).
% ligacao(31,43,2,1).
% ligacao(31,44,2,1).
% ligacao(32,41,2,3).
% ligacao(32,42,-1,0).
% ligacao(32,43,0,1).
% ligacao(32,44,1,2).
% ligacao(33,41,4,-1).
% ligacao(33,42,-1,3).
% ligacao(33,43,7,2).
% ligacao(33,44,5,-3).
% ligacao(34,41,3,2).
% ligacao(34,42,1,-1).
% ligacao(34,43,2,4).
% ligacao(34,44,1,-2).
% ligacao(41,200,2,0).
% ligacao(42,200,7,-2).
% ligacao(43,200,-2,4).
% ligacao(44,200,-1,-3).

% ligacao(1,51,6,2).
% ligacao(51,61,7,3).
% ligacao(61,200,2,4).

% sinonimos('c#','csharp').
% sinonimos(musica,muzica).

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
    
    
    
    
    
    % no(1,"ana",[natureza,pintura,musica,sw,porto]).
    % no(11,"antonio",[natureza,pintura,carros,futebol,lisboa]).
    % no(12,"beatriz",[natureza,musica,carros,porto,moda]).
    % no(13,"carlos",[natureza,musica,sw,futebol,coimbra]).
    % no(14,"daniel",[natureza,cinema,jogos,sw,moda]).
    % no(21,"eduardo",[natureza,cinema,teatro,carros,coimbra]).
    % no(22,"isabel",[natureza,musica,porto,lisboa,cinema]).
    % no(23,"jose",[natureza,pintura,sw,musica,carros,lisboa]).%
    % no(24,"luisa",[natureza,cinema,jogos,moda,porto]).
    % no(31,"maria",[natureza,pintura,musica,moda,porto]).%
    % no(32,"anabela",[natureza,cinema,musica,tecnologia,porto]).
    % no(33,"andre",[natureza,carros,futebol,coimbra]).
    % no(34,"catia",[natureza,musica,cinema,lisboa,moda]).
    % no(41,"cesar",[natureza,teatro,tecnologia,futebol,porto]).
    % no(42,"diogo",[natureza,futebol,sw,jogos,porto]).
    % no(43,"ernesto",[natureza,teatro,carros,porto]).
    % no(44,"isaura",[natureza,moda,tecnologia,cinema]).
    % no(200,"sara",[natureza,moda,musica,sw,coimbra]).
    
    % no(51,"rodolfo",[natureza,musica,sw]).
    % no(61,"rita",[moda,tecnologia,cinema]).
    
    
    % ligacao(1,11,10,8).
    % ligacao(1,12,2,6).
    % ligacao(1,13,-3,-2).
    % ligacao(1,14,1,-5).
    % ligacao(11,21,5,7).
    % ligacao(11,22,2,-4).
    % ligacao(11,23,-2,8).
    % ligacao(11,24,6,0).
    % ligacao(12,21,4,9).
    % ligacao(12,22,-3,-8).
    % ligacao(12,23,2,4).
    % ligacao(12,24,-2,4).
    % ligacao(13,21,3,2).
    % ligacao(13,22,0,-3).
    % ligacao(13,23,5,9).
    % ligacao(13,24,-2, 4).
    % ligacao(14,21,2,6).
    % ligacao(14,22,6,-3).
    % ligacao(14,23,7,0).
    % ligacao(14,24,2,2).
    % ligacao(21,31,2,1).
    % ligacao(21,32,-2,3).
    % ligacao(21,33,3,5).
    % ligacao(21,34,4,2).
    % ligacao(22,31,5,-4).
    % ligacao(22,32,-1,6).
    % ligacao(22,33,2,1).
    % ligacao(22,34,2,3).
    % ligacao(23,31,4,-3).
    % ligacao(23,32,3,5).
    % ligacao(23,33,4,1).
    % ligacao(23,34,-2,-3).
    % ligacao(24,31,1,-5).
    % ligacao(24,32,1,0).
    % ligacao(24,33,3,-1).
    % ligacao(24,34,-1,5).
    % ligacao(31,41,2,4).
    % ligacao(31,42,6,3).
    % ligacao(31,43,2,1).
    % ligacao(31,44,2,1).
    % ligacao(32,41,2,3).
    % ligacao(32,42,-1,0).
    % ligacao(32,43,0,1).
    % ligacao(32,44,1,2).
    % ligacao(33,41,4,-1).
    % ligacao(33,42,-1,3).
    % ligacao(33,43,7,2).
    % ligacao(33,44,5,-3).
    % ligacao(34,41,3,2).
    % ligacao(34,42,1,-1).
    % ligacao(34,43,2,4).
    % ligacao(34,44,1,-2).
    % ligacao(41,200,2,0).
    % ligacao(42,200,7,-2).
    % ligacao(43,200,-2,4).
    % ligacao(44,200,-1,-3).
    
    % ligacao(1,51,6,2).
    % ligacao(51,61,7,3).
    % ligacao(61,200,2,4).
    
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
    
    
    
    
    % Bibliotecas HTTP
    :- use_module(library(http/thread_httpd)).
    :- use_module(library(http/http_dispatch)).
    :- use_module(library(http/http_parameters)).
    :- use_module(library(http/http_client)).
    % Bibliotecas JSON
    :- use_module(library(http/json_convert)).
    :- use_module(library(http/http_json), [reply_json/2,http_read_json/3]).
    :- use_module(library(http/json)).
    :- use_module(library(http/http_cors)).
    :- use_module(library(settings)).
    :- use_module(library(http/http_error)).
    :- use_module(library(http/http_open)).
    :- set_setting(http:cors, [*]).
    
    % Read from another file database
    %:- include(bc_sprintB_rede_social).
    %:- include(users_X_commun_tags).
    %:- include(sgrai_prolog).
    :- debug.
    
    % Relação entre pedidos HTTP e predicados que os processam
    :- http_handler('/prolog/processa_json', p_json, []).
    :- json_object student(name:string, number:integer).
    
    % Relação entre pedidos HTTP e predicados que os processam
    :- http_handler('/prolog/atualizausers', update_users_json, []).
    
    % Relação entre pedidos HTTP e predicados que os processam
    :- http_handler('/prolog/atualizaconnections', update_connections_json, []).
    
    % Tamanho da Rede JSON
    :- http_handler('/prolog/tamanhorede', tamanho_rede_json, []).
    :- json_object rede(name:string, level:integer).
    
    :- http_handler('/prolog/listaadjacencias', listaadjacencias_json, []).
    :- json_object listaadjacencias_json_json(name:string, level:integer).
    
    :- http_handler('/prolog/sugereusers', sugere_conexoes_json, []).
    :- json_object rede(name:string, level:integer).
    
    % Caminhjo mais forte JSON
    :- http_handler('/prolog/caminhoforte', caminho_mais_forte, []).
    :- json_object rede(name:string, level:integer).
    
    
    % User com tags em comum JSON
    :- http_handler('/prolog/tagscomum', tagscomum_json, []).
    :- json_object tagscomum(x:integer).
    :- json_object tasgcomum_respostagerador(
    users_por_combinacao:list(users_por_combinacao_json/2)
    ).
    
    :- json_object users_por_combinacao_json(
            combinacao:list(string),
            users:list(string)
    ).
    
    
    
    p_json(Request) :- 
    http_read_json(Request, JSON, [json_object(dict)]),
    R = student(JSON.name,JSON.set_user),
    prolog_to_json(R, JSONObject),
    reply_json(JSONObject, [json_object(dict)]).
    
    
    sugere_conexoes_json(Request) :-
            get_usersFromDB,
            get_connectionsFromDB,
            http_read_json(Request, JSON, [json_object(dict)]),
            sugerir_conexao(JSON.name,JSON.level, R),
            prolog_to_json(R, JSONObject),
            reply_json(JSONObject, [json_object(dict)]).
    
    tamanho_rede_json(Request) :-
            get_usersFromDB,
            get_connectionsFromDB,
            http_read_json(Request, JSON, [json_object(dict)]),
            tamanhoRede(JSON.name, JSON.level, R),
            prolog_to_json(R, JSONObject),
            reply_json(JSONObject, [json_object(dict)]).
    
    
    :-json_object caminho_forca(
            forca:integer,
            caminho:list(string)
            ).
    
    caminho_mais_forte(Request) :-
            get_usersFromDB,
            get_connectionsFromDB,
           http_read_json(Request, JSON, [json_object(dict)]),
           plan_maisforte(JSON.name1,JSON.name2,Caminho,Forca),
           prolog_to_json(caminho_forca(Forca,Caminho), JSONObject),
           reply_json(JSONObject, [json_object(dict)]).
    
    
    
    
    tagscomum_json(Request) :-
            get_usersFromDB,
            get_connectionsFromDB,
            http_read_json(Request, JSON, [json_object(dict)]),
            users_X_commun_tags(JSON.x, L),
            prolog_to_json(tasgcomum_respostagerador(L), JSONObject),
            reply_json(JSONObject, [json_object(dict)]).
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    %Update users dinamicamente
    
    :-json_object users_be_json(
            users:list(user_be_json/17)
    ).
    
    
    :-json_object user_be_json(
            userId:integer,
            email:string,
            passwordStrength : string,
            dateOfBirth_D: integer,
            dateOfBirth_M: integer,
            dateOfBirth_Y: integer,
            firstName : string,
            lastName : string,
            phoneNumber: string,
            facebookLink:string,
            linkedinLink : string,
            emotionalState : string,
            emotionalStateChangeTime: string,
            city: string,
            country : string,
            briefDescription: string,
            tags:list(string)
    ).
    
    get_usersFromDB():-
            http_get('https://lapr5-networkmaster.herokuapp.com/User', Reply, [json_object(dict)]),
            retractall(no(_,_,_)),
            list_assertz(Reply.users).
    
    list_assertz([]).
    list_assertz([H|Tail]):- 
            assertz(no(H.userId,H.email,H.tags)), list_assertz(Tail).
    
    
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    %Update Ligações Automaticamente
    
    
    :-json_object connection_be_json(
            id : integer,   
            manConnectionStrength1:integer,
            manConnectionStrength2:integer,
            autoRelatStrength : integer,
            tagsUser1: list(string),
            tagsUser2: list(string),
            user1: integer, 
            user2 : integer
    ).
    
    :-json_object connections_be_json(
            connections:list(connection_be_json/8)
    ).
    
    
    get_connectionsFromDB():-
            http_get('https://lapr5-networkmaster.herokuapp.com/Connection', Reply, [json_object(dict)]),
            retractall(ligacao(_,_,_,_)),
            list_assertz_conn(Reply.connections).
    
    
    
    
    list_assertz_conn([]).
    list_assertz_conn([H|Tail]):- assertz(ligacao(H.user1,H.user2,H.manConnectionStrength1,H.manConnectionStrength2)), list_assertz_conn(Tail).
    
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    % RETURN USER ADJACENCY LIST
    
    :-json_object users_connected_level_json(
            usersList: list(list(list(list(string))))
    ).
    
    :-json_object users_connected_level_json(
            adjacencyList: list(users_json/1)
    ).
    :-json_object users_json(
            users : list(sub_users_json/1)
            ).
    
    :-json_object sub_users_json(
            sub_users :list(string)
    ).
    
    :-json_object integer_json(
            num : integer
    ).
    % :-json_object tuple_json(
    %         nome : string,
    %         forca : integer
    % ).
    
                    
     %usersList: [  adjacencyList : [   users:[ sub_users[ana] ] [ [antonio,10] , [joao,10]  ]                                   ] , [  [rodrigo]   ]    ]
    
    listaadjacencias_json(Request):-
            get_usersFromDB,
            get_connectionsFromDB,
            http_read_json(Request, JSON, [json_object(dict)]),
            listaadjacencias(JSON.name, JSON.level, Response),
            prolog_to_json(Response, JSONObject),
            reply_json(JSONObject, [json_object(dict)]).
    
    
    
        %% 1 camada intermédia , 1 nó
    %% Caminho mais forte Ana - António,  Eduardo  - 15
    
    % no(1,ana,[natureza,pintura,musica,sw,porto]).
    
    % no(11,antonio,[natureza,pintura,carros,futebol,lisboa]).
    
    % no(21,eduardo,[natureza,cinema,teatro,carros,coimbra]).
    
    % no(31,maria,[natureza,pintura,musica,moda,porto]).
    
    % no(41,cesar,[natureza,teatro,tecnologia,futebol,porto]).
    
    % no(200,sara,[natureza,moda,musica,sw,coimbra]).
    
    
    % ligacao(1,11,10,8).
    % ligacao(11,21,5,7).
    % ligacao(21,31,2,1).
    % ligacao(41,200,2,0).
    
    
    
    %% 2 camadas intermédias , 2 nós
    %% Caminho mais forte   [ana, antonio, eduardo, maria] - 17
    
    
    % no(1,ana,[natureza,pintura,musica,sw,porto]).
    
    % no(11,antonio,[natureza,pintura,carros,futebol,lisboa]).
    % no(12,beatriz,[natureza,musica,carros,porto,moda]).
    
    % no(21,eduardo,[natureza,cinema,teatro,carros,coimbra]).
    % no(22,isabel,[natureza,musica,porto,lisboa,cinema]).
    
    % no(31,maria,[natureza,pintura,musica,moda,porto]).
    
    
    % ligacao(1,11,10,8).
    % ligacao(1,12,2,6).
    
    % ligacao(11,21,5,7).
    % ligacao(11,22,2,-4).
    
    % ligacao(12,21,4,9).
    % ligacao(12,22,-3,-8).
    
    % ligacao(21,31,2,1).
    % ligacao(22,31,5,-4).
    
    %% 3 camadas intermédias , 3 nós
    %% Caminho mais forte   [ana, antonio, eduardo, maria, diogo, sara] 30
    
    % no(1,ana,[natureza,pintura,musica,sw,porto]).
    
    % no(11,antonio,[natureza,pintura,carros,futebol,lisboa]).
    % no(12,beatriz,[natureza,musica,carros,porto,moda]).
    % no(13,carlos,[natureza,musica,sw,futebol,coimbra]).
    
    % no(21,eduardo,[natureza,cinema,teatro,carros,coimbra]).
    % no(22,isabel,[natureza,musica,porto,lisboa,cinema]).
    % no(23,jose,[natureza,pintura,sw,musica,carros,lisboa]).
    
    % no(31,maria,[natureza,pintura,musica,moda,porto]).
    % no(32,anabela,[natureza,cinema,musica,tecnologia,porto]).
    % no(33,andre,[natureza,carros,futebol,coimbra]).
    
    % no(41,cesar,[natureza,teatro,tecnologia,futebol,porto]).
    % no(42,diogo,[natureza,futebol,sw,jogos,porto]).
    % no(43,ernesto,[natureza,teatro,carros,porto]).
    
    % no(200,sara,[natureza,moda,musica,sw,coimbra]).
    
    
    % ligacao(1,11,10,8).
    % ligacao(1,12,2,6).
    % ligacao(1,13,-3,-2).
    
    % ligacao(11,21,5,7).
    % ligacao(11,22,2,-4).
    % ligacao(11,23,-2,8).
    
    % ligacao(12,21,4,9).
    % ligacao(12,22,-3,-8).
    % ligacao(12,23,2,4).
    
    % ligacao(13,21,3,2).
    % ligacao(13,22,0,-3).
    % ligacao(13,23,5,9).
    
    % ligacao(21,31,2,1).
    % ligacao(21,32,-2,3).
    % ligacao(21,33,3,5).
    
    
    % ligacao(22,31,5,-4).
    % ligacao(22,32,-1,6).
    % ligacao(22,33,2,1).
    
    % ligacao(23,31,4,-3).
    % ligacao(23,32,3,5).
    % ligacao(23,33,4,1).
    
    % ligacao(31,41,2,4).
    % ligacao(31,42,6,3).
    % ligacao(31,43,2,1).
    
    % ligacao(32,41,2,3).
    % ligacao(32,42,-1,0).
    % ligacao(32,43,0,1).
    
    % ligacao(33,41,4,-1).
    % ligacao(33,42,-1,3).
    % ligacao(33,43,7,2).
    
    % ligacao(41,200,2,0).
    % ligacao(42,200,7,-2).
    % ligacao(43,200,-2,4).
    
    %% 4 camadas intermédias , 4 nós
    %% 
    
    % no(1,ana,[natureza,pintura,musica,sw,porto]).
    
    % no(11,antonio,[natureza,pintura,carros,futebol,lisboa]).
    % no(12,beatriz,[natureza,musica,carros,porto,moda]).
    % no(13,carlos,[natureza,musica,sw,futebol,coimbra]).
    % no(14,daniel,[natureza,cinema,jogos,sw,moda]).
    
    % no(21,eduardo,[natureza,cinema,teatro,carros,coimbra]).
    % no(22,isabel,[natureza,musica,porto,lisboa,cinema]).
    % no(23,jose,[natureza,pintura,sw,musica,carros,lisboa]).
    % no(24,luisa,[natureza,cinema,jogos,moda,porto]).
    
    % no(31,maria,[natureza,pintura,musica,moda,porto]).
    % no(32,anabela,[natureza,cinema,musica,tecnologia,porto]).
    % no(33,andre,[natureza,carros,futebol,coimbra]).
    % no(34,catia,[natureza,musica,cinema,lisboa,moda]).
    
    % no(41,cesar,[natureza,teatro,tecnologia,futebol,porto]).
    % no(42,diogo,[natureza,futebol,sw,jogos,porto]).
    % no(43,ernesto,[natureza,teatro,carros,porto]).
    % no(44,isaura,[natureza,moda,tecnologia,cinema]).
    
    % no(200,sara,[natureza,moda,musica,sw,coimbra]).
    
    
    % ligacao(1,11,10,8).
    % ligacao(1,12,2,6).
    % ligacao(1,13,-3,-2).
    % ligacao(1,14,1,-5).
    % ligacao(11,21,5,7).
    % ligacao(11,22,2,-4).
    % ligacao(11,23,-2,8).
    % ligacao(11,24,6,0).
    % ligacao(12,21,4,9).
    % ligacao(12,22,-3,-8).
    % ligacao(12,23,2,4).
    % ligacao(12,24,-2,4).
    % ligacao(13,21,3,2).
    % ligacao(13,22,0,-3).
    % ligacao(13,23,5,9).
    % ligacao(13,24,-2, 4).
    % ligacao(14,21,2,6).
    % ligacao(14,22,6,-3).
    % ligacao(14,23,7,0).
    % ligacao(14,24,2,2).
    % ligacao(21,31,2,1).
    % ligacao(21,32,-2,3).
    % ligacao(21,33,3,5).
    % ligacao(21,34,4,2).
    % ligacao(22,31,5,-4).
    % ligacao(22,32,-1,6).
    % ligacao(22,33,2,1).
    % ligacao(22,34,2,3).
    % ligacao(23,31,4,-3).
    % ligacao(23,32,3,5).
    % ligacao(23,33,4,1).
    % ligacao(23,34,-2,-3).
    % ligacao(24,31,1,-5).
    % ligacao(24,32,1,0).
    % ligacao(24,33,3,-1).
    % ligacao(24,34,-1,5).
    % ligacao(31,41,2,4).
    % ligacao(31,42,6,3).
    % ligacao(31,43,2,1).
    % ligacao(31,44,2,1).
    % ligacao(32,41,2,3).
    % ligacao(32,42,-1,0).
    % ligacao(32,43,0,1).
    % ligacao(32,44,1,2).
    % ligacao(33,41,4,-1).
    % ligacao(33,42,-1,3).
    % ligacao(33,43,7,2).
    % ligacao(33,44,5,-3).
    % ligacao(34,41,3,2).
    % ligacao(34,42,1,-1).
    % ligacao(34,43,2,4).
    % ligacao(34,44,1,-2).
    % ligacao(41,200,2,0).
    % ligacao(42,200,7,-2).
    % ligacao(43,200,-2,4).
    % ligacao(44,200,-1,-3).
    
    %% caminho mais forte
    
    
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
    
    % users_X_commun_tags(CurrentUser,X,List_Result):-
    %     % get_time(T1),
    %     no(_,CurrentUser,Tags),
    %     verificar_sinonimos(Tags,Tags_Com_Sinonimos),
    %     findall(User,(no(_,User,_),User \== CurrentUser),Users),
    %     users_X_commun_tags2(X,Tags_Com_Sinonimos,Users,List_Result).
    %     % write('Solucao encontrada em '),
    %     % get_time(T2),
    %     % T is T2-T1,write(T),write(' segundos'),nl.
    
    % users_X_commun_tags2(_,_,[],[]):-!.
    % users_X_commun_tags2(X,Tags,[U|Users],Result):-
    %     no(_,U,User_Tags),
    %     intersection(Tags, User_Tags,Commun),
    %     length(Commun, Size),
    %     Size >= X, !,
    %     users_X_commun_tags2(X,Tags,Users,Result1),
    %     append([U], Result1, Result).
    % users_X_commun_tags2(X,Tags,[_|Users],Result):-
    %     !,
    %     users_X_commun_tags2(X,Tags,Users,Result).
    
    % verificar_sinonimos([],[]):-!.
    % verificar_sinonimos([Tag|List_Tags],List_Sinonimos):-
    %     (sinonimos(Tag,Sin);sinonimos(Sin,Tag)),
    %     verificar_sinonimos(List_Tags,List_Sinonimos1),!,
    %     append([Tag,Sin], List_Sinonimos1, List_Sinonimos).
    % verificar_sinonimos([Tag|List_Tags],List_Sinonimos):-
    %     verificar_sinonimos(List_Tags,List_Sinonimos1),!,
    %     append([Tag], List_Sinonimos1, List_Sinonimos).
    
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

:-multifile(some_fact/1).
some_fact(main).
some_fact(master).

