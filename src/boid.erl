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
-module(boid).

-export([start/1]).
-export([state/6]).
-export([boid/1]).

-record(state, {buffer_pid :: pid(),
                heatmap_pid :: pid(),
                shape :: atom(),
                max_height :: integer(),
                max_width :: integer(),
                rgba :: tuple(integer(), integer(), integer(), float()),
                x :: integer(),
                y :: integer()}).

-define(CYCLE_TIME, 50).
-define(BOID_SIZE, 10).

state(BufferPid, HeatMapPid, Shape, MaxHeight, MaxWidth, MaxRGB) ->
    _ = random:seed(os:timestamp()),
    {X, Y} = {random:uniform(MaxWidth),random:uniform(MaxHeight)},
    [R, G, B] = [rand_color_elem(Elem) || Elem <- MaxRGB],
    #state{buffer_pid = BufferPid,
           heatmap_pid = HeatMapPid,
           shape = Shape,
           max_height = MaxHeight,
           max_width = MaxWidth,
           rgba = {R, G, B, 1.0},
           x = X,
           y = Y}.

start(State = #state{heatmap_pid = HeatMapPid, x = X, y = Y}) ->
    heatmap:insert(HeatMapPid, point2grid({X, Y}, ?BOID_SIZE)),
    boid(State).

boid(State = #state{buffer_pid = BufferPid,
                    heatmap_pid = HeatMapPid,
                    shape = Shape,
                    x = OldX,
                    y = OldY,
                    rgba = RGBA}) ->

    BufferPid ! {self(), shape:shape(Shape, {OldX, OldY}, ?BOID_SIZE, RGBA)},
    erlang:send_after(?CYCLE_TIME, self(), draw),
    receive
        draw ->
            {NewX, NewY} = next_point(OldX, OldY, HeatMapPid, State),

            %NewX = (OldX + 5) rem State#state.max_width,
            %NewY = (OldY + 5) rem State#state.max_height,
            heatmap:move(HeatMapPid,
                         point2grid({OldX, OldY}, ?BOID_SIZE),
                         point2grid({NewX, NewY}, ?BOID_SIZE)),
            boid(State#state{x = NewX, y = NewY});
        Other ->
            io:format("boid ~p received ~p~n", [self(), Other]),
            ok
    after 5000 ->
        io:format("boid ~p didn't receive anything~n", [self()]),
        ok
    end.

next_point(OldX, OldY, HeatMapPid, State) ->
    MoveDist = 3,
    Heat = heatmap:heat(HeatMapPid, point2grid({OldX, OldY}, ?BOID_SIZE)),
    ValidMoves = lists:filter(valid_move_filter(OldX, OldY,
                                                MoveDist,
                                                State#state.max_width,
                                                State#state.max_height),
                              Heat),
    SortedHeat = lists:sort(fun({_, H1}, {_, H2}) -> H1 > H2 end, ValidMoves),
    {XMultiple, YMultiple} = xy_multiples(SortedHeat),
    NewX = (OldX + (XMultiple * MoveDist)) rem State#state.max_width,
    NewY = (OldY + (YMultiple * MoveDist)) rem State#state.max_height,
    {NewX, NewY}.

xy_multiples([]) ->
    Multipliers = [{X2, Y2} || X2 <- [-1, 0, 1], Y2 <- [-1, 0, 1], {X2, Y2} /= {0, 0}],
    lists:nth(random:uniform(8), Multipliers);
xy_multiples(XYHeat) ->
    case [XYH || XYH = {_, H} <- XYHeat, H =< 60] of
        [] ->
            element(1, hd(lists:reverse(XYHeat)));
        [{X, Y}, _] ->
            {X, Y};
        [{_, MaxValid} | _] = ValidHeat ->
            BestHeat = lists:filter(fun({_, H}) -> H == MaxValid end, ValidHeat),
            io:format("boid:xy_multiples(...);~n\tBestHeat = ~p~n", [BestHeat]),
            element(1,lists:nth(random:uniform(length(BestHeat)), BestHeat))
    end.

valid_move_filter(X1, Y1, MoveDist, MaxWidth, MaxHeight) ->
    fun({{XMult, YMult}, _Heat}) ->
        X2 = X1 + (XMult * MoveDist),
        Y2 = Y1 + (YMult * MoveDist),
        X2 > 0 andalso X2 < MaxWidth andalso Y2 > 0 andalso Y2 < MaxHeight
    end.

point2grid({X, Y}, CellSize) ->
    ToCenter = CellSize div 2,
    {(X + ToCenter) div CellSize, (Y + ToCenter) div CellSize}.

rand_color_elem(0) ->
    0;
rand_color_elem(X) ->
    random:uniform(X).
