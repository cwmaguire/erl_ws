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

-define(CYCLE_TIME, 1000).
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

    %{H, W} = {?BOID_SIZE, ?BOID_SIZE},
    BufferPid ! shape:shape(Shape, {OldX, OldY}, ?BOID_SIZE, RGBA),
    erlang:send_after(?CYCLE_TIME, self(), draw),
    receive
        draw ->
            NewX = (OldX + 5) rem State#state.max_width,
            NewY = (OldY + 5) rem State#state.max_height,
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

%point2grid(Point, #state{max_width = MaxW, max_height = MaxH}) ->
point2grid({X, Y}, CellSize) ->
    ToCenter = CellSize div 2,
    {(X + ToCenter) div CellSize, (Y + ToCenter) div CellSize}.

rand_color_elem(0) ->
    0;
rand_color_elem(X) ->
    random:uniform(X).
