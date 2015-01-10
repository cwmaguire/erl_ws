-module(hello_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

%orig ("make new t=cowboy_http n=hello_handler")
%handle(Req, State=#state{}) ->
	%{ok, Req2} = cowboy_req:reply(200, Req),
	%{ok, Req2, State}.

%web example: http://ninenines.eu/docs/en/cowboy/1.0/guide/getting_started/
handle(Req, State=#state{}) ->
    {ok, Req2} = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/plain">>}],
        <<"Hello Erlang!">>,
        Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
