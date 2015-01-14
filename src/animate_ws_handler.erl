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

%% Route drawing commands to and from a web page to an Erlang process
-module(animate_ws_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {animator_pid :: pid()}).

%parse(Binary = <<"height", _/binary>>, State) ->
parse(<<"height:", Height0/binary>>) ->
    Height = list_to_integer(binary_to_list(Height0)),
    {animate, height, [Height]};
parse(<<"width:", Width0/binary>>) ->
    Width = list_to_integer(binary_to_list(Width0)),
    {animate, width, [Width]};
parse(_) ->
    undefined.

init(_, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_Type, Req, _Opts) ->
    Req3 = case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req) of
        {ok, undefined, Req2} ->
            Req2;
        {ok, Subprotocols, Req2} ->
            io:format("Subprotocols found: ~p~n", [Subprotocols]),
            Req2
    end,
    _ = supervisor:start_child(erl_ws_sup, [self()]),
    io:format("Websocket handler init (~p)~n", [self()]),
    {ok, Req3, #state{}}.

websocket_handle({text, StartStop}, Req, State) when StartStop == <<"start">>; StartStop == <<"stop">> ->
    io:format("From Websocket: {text, ~p}~n", [StartStop]),
    animate:(list_to_atom(binary_to_list(StartStop)))(State#state.animator_pid),
    {reply, {text, ["Erlang received command: ", StartStop]}, Req, State};
websocket_handle({text, FrameContent}, Req, State) ->
    io:format("From Websocket: {text, ~p}~n", [FrameContent]),
    case parse(FrameContent) of
        undefined ->
            ok;
        {M, F, A} ->
            erlang:apply(M, F, [State#state.animator_pid | A])
    end,
    {ok, Req, State};
websocket_handle({FrameType, FrameContent}, Req, State) ->
    io:format("From Websocket (non-text): {~p, ~p}~n", [FrameType, FrameContent]),
    {reply, {text, ["Erlang received: ", FrameContent, " of type ", atom_to_list(FrameType)]}, Req, State}.

websocket_info({animator, Pid}, Req, State) ->
    io:format("Websocket handler (~p) received animator pid (~p)~n", [self(), Pid]),
    {ok, Req, State#state{animator_pid = Pid}};
websocket_info(ErlangMessage, Req, State) ->
    %io:format("From Erlang (presumably from ~p): ~p~n", [State#state.animator_pid, ErlangMessage]),
    {reply, {text, [ErlangMessage]}, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

handle(Req, State=#state{}) ->
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
	ok.
