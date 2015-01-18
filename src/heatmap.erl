-module(heatmap).
-behaviour(gen_server).

-export([start/0]).
-export([stop/1]).
-export([insert/2]).
-export([move/3]).
-export([heat/2]).
-export([heatmap/1]).
-export([render/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-record(state, {cells :: dict()}).

-define(CYCLE_TIME, 1000).
-define(RANGE, 6).
-define(FALLOFF, 30).

%% API

start() ->
    {ok, Pid} = gen_server:start_link(?MODULE, {}, _Options = []),
    Pid.

stop(Pid) ->
    gen_server:cast(Pid, stop).

insert(Pid, To) ->
    gen_server:cast(Pid, {To}).

move(Pid, From, To) ->
    gen_server:cast(Pid, {From, To}).

heat(Pid, XY) ->
    gen_server:call(Pid, {heat, XY}).

render(Pid) ->
    gen_server:call(Pid, render).

heatmap(Pid) ->
    gen_server:call(Pid, map).

%% Internal

update(UpdateFunFun, {X, Y}, State = #state{cells = Cells}) ->
    Amt = ?RANGE * ?FALLOFF,
    MaxMinXYs = [{X - N, Y - N, X + N, Y + N} || N <- lists:seq(0, ?RANGE - 1)],
    {_, _, NewCells}  = lists:foldl(fun update_square/2,
                                    {UpdateFunFun, Amt, Cells},
                                    MaxMinXYs),
    State#state{cells = dict:filter(fun(_, V) -> V > 0 end, NewCells)}.

update_square(MaxMinXY, Acc = {UpdateFunFun, Amt, _Cells}) ->
    SquareCells = square(MaxMinXY),
    {_, _, NewCells} = lists:foldl(fun update_cell/2, Acc, SquareCells),
    {UpdateFunFun, Amt - ?FALLOFF, NewCells}.

square({X1, Y1, X2, Y2}) ->
    Xs = lists:seq(X1, X2),
    Ys = lists:seq(Y1, Y2),
    [{X, Y} || X <- Xs, Y <- Ys, X == X1 orelse X == X2 orelse Y == Y1 orelse Y == Y2].

update_cell({X, Y}, {UpdateFunFun, Amt, Cells}) ->
    {UpdateFunFun, Amt, dict:update({X, Y}, UpdateFunFun(Amt), Amt, Cells)}.

add_fun(Amt) ->
    fun(X) -> X + Amt end.

rem_fun(Amt) ->
    fun(X) -> X - Amt end.

render_cells(Cells) ->
    RenderedCells = dict:fold(fun render_cell/3, [], Cells),
    [{canvas, <<"heatmap">>}, {objs, RenderedCells}].

render_cell({X, Y}, Amt, Objects) ->
    Red = 255 - Amt,
    Cell = shape:shape(rectangle, {X * 10, Y * 10}, 10, {Red, 0, 0, 1.0}),
    [Cell | Objects].

heat_({X, Y}, Cells) ->
    SurroundingPoints = [{X2, Y2} || X2 <- [X - 1, X, X + 1], Y2 <- [Y - 1, Y, Y + 1], {X2, Y2} /= {X, Y}],
    io:format("heatmap:heat_({~p, ~p}, Cells);~n\tSurroundingPoints = ~p~n", [X, Y, SurroundingPoints]),
    [{{X2 - X, Y2 - Y}, Amt} || {X2, Y2} <- SurroundingPoints, {ok, Amt} <- [dict:find({X2, Y2}, Cells)]].

%% gen_server

init({}) ->
    io:format("heatmap gen_server init (~p) ~n", [self()]),
    {ok, #state{cells = dict:new()}}.

handle_call({heat, {X, Y}}, _From, State = #state{cells = Cells}) ->
    {reply, heat_({X, Y}, Cells), State};
handle_call(map, _From, State) ->
    {reply, State#state.cells, State};
handle_call(render, _From, State = #state{cells = Cells}) ->
    {reply, render_cells(Cells), State};
handle_call(Request, From, State) ->
    io:format("heatmap:handle_call(~p, ~p, State)~n", [Request, From]),
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({To}, State) ->
    {noreply, update(fun add_fun/1, To, State)};
handle_cast({From, To}, State) ->
    {noreply, update(fun add_fun/1, To, update(fun rem_fun/1, From, State))}.

handle_info(Info, State) ->
    io:format("heatmap:handle_info(~p, ~p)~n", [Info, State]),
    {noreply, State}.

code_change(_OldVersion, State, _Version) -> {ok, State}.

terminate(_Reason, _State) ->
    ok.
