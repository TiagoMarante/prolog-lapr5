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

:-multifile(some_fact/1).
some_fact(main).
some_fact(master).

