-module(hello_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

init(_, Req, _Opts) ->
    %io:format("Hellow world init~n"),
	{ok, Req, #state{}}.

%orig ("make new t=cowboy_http n=hello_handler")
%handle(Req, State=#state{}) ->
	%{ok, Req2} = cowboy_req:reply(200, Req),
	%{ok, Req2, State}.

% To do nothing (not even send response, e.g. 200)
% (Maybe useful for recording visits to invalid URLs,
%  or possibly setting up a session on the server without giving
%  any indication of doing so)
%handle(Req, State) ->
%    {ok, Req, State}.

%web example: http://ninenines.eu/docs/en/cowboy/1.0/guide/getting_started/
handle(Req, State=#state{}) ->
    {ok, Req2} = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/plain">>}],
        <<"Hello Erlang!">>,
        Req),
    {ok, Req2, State}.

% Used to clean up any resources, messages, timers, etc.
terminate(_Reason, _Req, _State) ->
	ok. % can't return anything else
