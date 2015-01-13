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

-export([start/0]).
-export([stop/0]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-define(NO_ARGS, []).

-record(state, {running = false :: boolean()}).

start() ->
    gen_server:cast(animate, start).

stop() ->
    gen_server:cast(animate, stop).

init(?NO_ARGS) ->
    {ok, #state{}}.

handle_call(Request, From, State) ->
    io:format("animate:handle_call(~p, ~p, ~p)~n", [Request, From, State]),
    {reply, the_reply, State}.

handle_cast(start, State = #state{running = false}) ->
    io:format("animate:handle_cast(start, ~p)~n", [State]),
    self() ! animate,
    {noreply, State#state{running = true}};
handle_cast(stop, State = #state{running = true}) ->
    io:format("animate:handle_cast(stop, ~p)~n", [State]),
    {noreply, State#state{running = false}};
handle_cast(Request, State) ->
    io:format("animate:handle_cast(~p, ~p)~n", [Request, State]),
    {noreply, State}.

handle_info(animate, State = #state{running = true}) ->
    io:format("animating!~n"),
    random:seed(os:timestamp()),
    {X, Y} = {random:uniform(200), random:uniform(200)},
    animate_ws ! ["{", integer_to_list(X), ",", integer_to_list(Y), "}"],
    timer:sleep(100),
    self() ! animate,
    {noreply, State};
handle_info(Info, State) ->
    io:format("animate:handle_info(~p, ~p)~n", [Info, State]),
    {noreply, State}.

code_change(_OldVersion, _State, _Version) -> ok.

terminate(_Reason, _State) ->
    ok.
