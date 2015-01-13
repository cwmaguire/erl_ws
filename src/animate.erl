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
-module(animate).
-behaviour(gen_server).

-export([start/1]).
-export([stop/1]).
-export([send/2]).
-export([height/2]).
-export([width/2]).
-export([start_gen_server/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-define(NO_ARGS, []).

-record(state, {running = false :: boolean(),
                animate_websocket_pid :: pid(),
                height :: integer(),
                width :: integer()}).

%% public interface

start(Pid) ->
    gen_server:cast(Pid, start).

stop(Pid) ->
    gen_server:cast(Pid, stop).

send(Pid, Text) ->
    gen_server:call(Pid, {text, Text}).

height(Pid, Height) ->
    gen_server:cast(Pid, {height, Height}).

width(Pid, Width) ->
    gen_server:cast(Pid, {width, Width}).

%% gen_server logic

start_gen_server(AnimateWebSocketPid) ->
    _ok_pid = gen_server:start_link(?MODULE, AnimateWebSocketPid, _Options = []).

init(AnimateWebsocketPid) ->
    io:format("animate gen_server init (~p, ~p) ~n", [self(), AnimateWebsocketPid]),
    AnimateWebsocketPid ! {animator, self()},
    {ok, #state{animate_websocket_pid = AnimateWebsocketPid}}.

handle_call(Request, From, State) ->
    io:format("animate:handle_call(~p, ~p, ~p)~n", [Request, From, State]),
    {reply, ok, State}.

handle_cast(start, State = #state{running = false}) ->
    io:format("animate:handle_cast(start, ~p)~n", [State]),
    self() ! animate,
    {noreply, State#state{running = true}};
handle_cast(stop, State = #state{running = true}) ->
    io:format("animate:handle_cast(stop, ~p)~n", [State]),
    {noreply, State#state{running = false}};
handle_cast({height, Height}, State) ->
    io:format("animate:handle_cast({height, ~p}, ~p)~n", [Height, State]),
    {noreply, State#state{height = Height}};
handle_cast({width, Width}, State) ->
    io:format("animate:handle_cast({width, ~p}, ~p)~n", [Width, State]),
    {noreply, State#state{width = Width}};
handle_cast(Request, State) ->
    io:format("animate:handle_cast(~p, ~p)~n", [Request, State]),
    {noreply, State}.

handle_info(animate, State = #state{running = true, height = Height, width = Width}) ->
    io:format("animating!~n"),
    random:seed(os:timestamp()),
    {X, Y} = {random:uniform(Height), random:uniform(Width)},
    [R, G, B] = [random:uniform(255) || _ <- lists:seq(1,3)],
    Alpha = (random:uniform(9) + 1) / 10,
    JSON = io_lib:format("{\"type\":\"square\",\"x\":~b,\"y\":~b,\"r\":~b,\"g\":~b,\"b\":~b,\"a\":~f}",
                         [X, Y, R, G, B, Alpha]),
    State#state.animate_websocket_pid ! JSON,
    %State#state.animate_websocket_pid ! ["{", integer_to_list(X), ",", integer_to_list(Y), "}"],
    timer:sleep(100),
    self() ! animate,
    {noreply, State};
handle_info(Info, State) ->
    io:format("animate:handle_info(~p, ~p)~n", [Info, State]),
    {noreply, State}.

code_change(_OldVersion, _State, _Version) -> ok.

terminate(_Reason, _State) ->
    ok.
