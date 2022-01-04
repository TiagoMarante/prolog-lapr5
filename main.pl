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
:- use_module(library(http/http_ssl_plugin)).

:- set_setting(http:cors, [*]).


% Read from another file database
:- include(bc_sprintB_rede_social).
:- include(users_X_commun_tags).
:- include(sgrai_prolog).


% Rotas
:- http_handler(/, root_handler, []).
:- http_handler('/db', db, []).

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
        %:-json_object tuple_json(
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

root_handler(_):-
        format('Content-Type: text/html~n~n', []),
        findall(Fact, some_fact(Fact), Facts),
        atomic_list_concat(Facts, ', ', Atom),
        format('Hello from Prolog: ~w', [Atom]).

db(_):-
        heroku_db_connect(Connection),
        format('Content-Type: text/html~n~n', []),        
        format('We have a connection: ~q', [Connection]).

:-multifile(some_fact/1).
some_fact(main).
some_fact(master).

