%% Copyright (c) 2015, Chris Maguire <cwmaguire@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
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
