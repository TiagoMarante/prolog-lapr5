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


% Read from another file database
:- include(bc_sprintB_rede_social).
:- include(users_X_commun_tags).
:- include(sgrai_prolog).

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

