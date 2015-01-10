-module(erl_ws_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

%orig
%start(_Type, _Args) ->
	%erl_ws_sup:start_link().

%cowboy
start(_Type, _Args) ->
    AnyHost = '_',
    Path = "/",
    Handler = hello_handler,
    Bindings = [],
    Paths = [{Path, Handler, Bindings}],
    Routes = [{AnyHost, Paths}],
    Dispatch = cowboy_router:compile(Routes),
    %Dispatch = cowboy_router:compile([{'_', [{"/", hello_handler, []}]} ]),
    cowboy:start_http(my_http_listener, 100, [{port, 8080}], [{env, [{dispatch, Dispatch}]}]),
    %hello_erlang_sup:start_link().
    erl_ws_sup:start_link().

stop(_State) ->
	ok.
