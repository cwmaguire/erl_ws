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

%% Demonstrate websocket handling
-module(websocket_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

init(_, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_Type, Req, _Opts) ->
    io:format("Websocket handler init~n"),
    Req3 = case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req) of
        {ok, undefined, Req2} ->
            Req2;
        {ok, Subprotocols, Req2} ->
            io:format("Subprotocols found: ~p~n", [Subprotocols]),
            Req2
    end,
    register(websocket, self()),
    %self() ! "message sent from init",
    websocket ! "message sent from init",
    {ok, Req3, #state{}}.

websocket_handle({FrameType, FrameContent}, Req, State) ->
    io:format("From Websocket: {~p, ~p}~n", [FrameType, FrameContent]),
    {reply, {text, ["Received from Websocket: ", FrameContent, " of type ", atom_to_list(FrameType)]}, Req, State}.

websocket_info(ErlangMessage, Req, State) ->
    io:format("From Erlang: ~p~n", [ErlangMessage]),
    {reply, {text, ["Received from Erlang: ", ErlangMessage]}, Req, State}.

handle(Req, State=#state{}) ->
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
	ok.
