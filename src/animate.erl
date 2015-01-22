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
                boids = [] :: list(pid()),
                animate_websocket_pid :: pid(),
                buffer_pid :: pid(),
                heatmap_pid :: pid(),
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
    _ = random:seed(os:timestamp()),
    AnimateWebsocketPid = State#state.animate_websocket_pid,
    HeatMapPid = heatmap:start(),
    self() ! render_heatmap,
    BufferPid = spawn(buffer, start, [AnimateWebsocketPid, cycle_time()]),
    MaxHeight = State#state.height,
    MaxWidth = State#state.width,
    Specs = [{ellipse, [255,0,0]},
             {ellipse, [0,255,0]},
             {ellipse, [255,255,0]},
             {ellipse, [0,255,255]},
             {ellipse, [100,200,100]},
             {rectangle, [0,0,255]},
             {rectangle, [100,20,200]},
             {rectangle, [30,90,200]},
             {rectangle, [30,90,200]},
             {rectangle, [30,90,200]},
             {packman, [100,255,200]}],

    Pids = [spawn(fun() -> boid:start(boid:state(BufferPid, HeatMapPid, Shape, MaxHeight, MaxWidth, RGB))
                  end) || {Shape, RGB} <- Specs],

    io:format("animate started boids: ~p~n", [Pids]),
    {noreply, State#state{running = true,
                          boids=Pids,
                          buffer_pid = BufferPid,
                          heatmap_pid = HeatMapPid}};
handle_cast(stop, State = #state{running = true,
                                 boids = Boids,
                                 buffer_pid = BufferPid,
                                 heatmap_pid = HeatMapPid}) ->
    io:format("animate:handle_cast(stop, ~p)~n", [State]),
    heatmap:stop(HeatMapPid),
    _ = [Boid ! stop || Boid <- Boids],
    BufferPid ! stop,
    {noreply, State#state{running = false}};
handle_cast({height, Height}, State) ->
    {noreply, State#state{height = Height}};
handle_cast({width, Width}, State) ->
    {noreply, State#state{width = Width}};
handle_cast(Request, State) ->
    io:format("animate:handle_cast(~p, ~p)~n", [Request, State]),
    {noreply, State}.

handle_info(render_heatmap, State = #state{heatmap_pid = HeatmapPid}) ->
    Heatmap = heatmap:render(HeatmapPid),
    JSON = jsx:encode(Heatmap),
    State#state.animate_websocket_pid ! JSON,
    erlang:send_after(cycle_time(), self(), render_heatmap),
    {noreply, State};
handle_info(Info, State) ->
    io:format("animate:handle_info(~p, ~p)~n", [Info, State]),
    {noreply, State}.

code_change(_OldVersion, State, _Version) -> {ok, State}.

terminate(_Reason, _State) ->
    ok.

cycle_time() ->
    {ok, CycleTime} = application:get_env(erl_ws, cycle_time),
    CycleTime.
