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

-record(cell, {amt :: integer(),
               dissipation_cycles = 0 :: integer()}).

-define(RANGE, 6).
-define(FALLOFF, 10).
-define(DISSIPATION_CYCLES, 2).

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

update(UpdateFunFun, {X, Y}, Cells) ->
    Amt = ?RANGE * ?FALLOFF,
    MaxMinXYs = [{X - N, Y - N, X + N, Y + N} || N <- lists:seq(0, ?RANGE - 1)],
    {_, _, NewCells}  = lists:foldl(fun update_square/2,
                                    {UpdateFunFun, Amt, Cells},
                                    MaxMinXYs),
    dict:filter(fun(_, Cell) -> Cell#cell.amt > 0 end, NewCells).

update_square(MaxMinXY, Acc = {UpdateFunFun, Amt, _Cells}) ->
    SquarePoints = square(MaxMinXY),
    {_, _, NewCells} = lists:foldl(fun update_cell/2, Acc, SquarePoints),
    {UpdateFunFun, Amt - ?FALLOFF, NewCells}.

square({X1, Y1, X2, Y2}) ->
    Xs = lists:seq(X1, X2),
    Ys = lists:seq(Y1, Y2),
    [{X, Y} || X <- Xs, Y <- Ys, X == X1 orelse X == X2 orelse Y == Y1 orelse Y == Y2].

update_cell(Point, {UpdateFunFun, Amt, Cells}) ->
    NewCells = dict:update(Point, UpdateFunFun(Amt), #cell{amt = Amt}, Cells),
    {UpdateFunFun, Amt, NewCells}.

add_fun(Add) ->
    fun(Cell = #cell{amt = Amt}) ->
        Cell#cell{amt = Amt + Add}
    end.

rem_fun(Remove) ->
    fun(Cell = #cell{amt = Amt,
                     dissipation_cycles = DissipationCycles}) ->
        NewDissipationCycles = round(Amt / ?FALLOFF) + 1,
        Cell#cell{amt = Amt - round((Remove / 2)),
                  dissipation_cycles = DissipationCycles + NewDissipationCycles}
    end.

render_cells(Cells) ->
    RenderedCells = dict:fold(fun render_cell/3, [], Cells),
    [{canvas, <<"heatmap">>}, {objs, RenderedCells}].

render_cell({X, Y}, #cell{amt = Amt}, Objects) ->
    Red = max(0, 255 - Amt),
    Cell = shape:shape(rectangle, {X * 10, Y * 10}, 10, {Red, 0, 0, 1.0}, gradient, {Red + ?FALLOFF, 0, 0, 1.0}),
    [Cell | Objects].

heat_({X, Y}, Cells) ->
    SurroundingPoints = [{X2, Y2} || X2 <- [X - 1, X, X + 1], Y2 <- [Y - 1, Y, Y + 1], {X2, Y2} /= {X, Y}],
    [{{X2 - X, Y2 - Y}, Amt} || {X2, Y2} <- SurroundingPoints, {ok, Amt} <- [dict:find({X2, Y2}, Cells)]].

dissipate(Cells) ->
    dict:filter(fun has_amount/2, dict:map(fun dissipate_cell/2, Cells)).

dissipate_cell(_, Cell = #cell{amt = Amt,
                               dissipation_cycles = DissipationCycles})
    when is_integer(DissipationCycles), DissipationCycles > 0 ->

    Dissipation = ?FALLOFF,
    NewAmt = case Amt - Dissipation of
                 X when X < 0 ->
                     0;
                 X ->
                     X
             end,
    Cell#cell{amt = NewAmt,
              dissipation_cycles = max(0, DissipationCycles - 1)};
dissipate_cell(_, Cell) ->
    Cell.

has_amount(_Key, #cell{amt = Amt}) ->
    Amt > 0.

cycle_time() ->
    {ok, CycleTime} = application:get_env(erl_ws, cycle_time),
    CycleTime.

%% gen_server

init({}) ->
    self() ! dissipate,
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
    Cells = update(fun add_fun/1, To, State#state.cells),
    {noreply, State#state{cells = Cells}};
handle_cast({From, To}, State = #state{cells = Cells}) ->
    NewCells = update(fun add_fun/1, To, update(fun rem_fun/1, From, Cells)),
    {noreply, State#state{cells = NewCells}}.

handle_info(dissipate, State = #state{cells = Cells}) ->
    erlang:send_after(cycle_time(), self(), dissipate),
    {noreply, State#state{cells = dissipate(Cells)}};
handle_info(Info, State) ->
    io:format("heatmap:handle_info(~p, ~p)~n", [Info, State]),
    {noreply, State}.

code_change(_OldVersion, State, _Version) -> {ok, State}.

terminate(_Reason, _State) ->
    ok.
