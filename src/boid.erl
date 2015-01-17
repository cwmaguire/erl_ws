-module(boid).

-export([spec/4]).
-export([boid/2]).

-record(spec, {shape :: atom(),
               max_height :: integer(),
               max_width :: integer(),
               rgb :: tuple(integer(), integer(), integer()),
               x :: integer(),
               y :: integer()}).

spec(Shape, MaxHeight, MaxWidth, MaxRGB) ->
    _ = random:seed(os:timestamp()),
    {X, Y} = {random:uniform(MaxWidth),random:uniform(MaxHeight)},
    RGB = [rand_color_elem(Elem) || Elem <- MaxRGB],
    #spec{shape = Shape,
          max_height = MaxHeight,
          max_width = MaxWidth,
          rgb = RGB,
          x = X,
          y = Y}.

boid(OutPid, Spec = #spec{shape = Shape,
                  x = X,
                  y = Y,
                  rgb = RGB}) ->

    OutPid ! shapeJSON(Shape, X, Y, RGB, 1.0),
    erlang:send_after(100, self(), draw),
    receive
        draw ->
            NewX = (X + 5) rem Spec#spec.max_width,
            NewY = (Y + 5) rem Spec#spec.max_height,
            boid(OutPid, Spec#spec{x = NewX, y = NewY});
        Other ->
            io:format("boid ~p received ~p~n", [self(), Other]),
            ok
    after 5000 ->
        io:format("boid ~p didn't receive anything~n", [self()]),
        ok
    end.

shapeJSON(rectangle, X, Y, [R, G, B], Alpha) ->
    %{H, W} = {random:uniform(10), random:uniform(10)},
    {H, W} = {10, 10},
    jsx:encode([{shape, rectangle},
                {x, X}, {y, Y},
                {h, H}, {w, W},
                {r, R}, {g, G}, {b, B}, {a, Alpha}]);
shapeJSON(ellipse, X, Y, [R, G, B], Alpha) ->
    %Radius = random:uniform(random:uniform(10)),
    Radius = 10,
    jsx:encode([{shape, ellipse},
                {x, X}, {y, Y},
                {rad, Radius},
                {start, 0}, {'end', 2 * math:pi()},
                {r, R}, {g, G}, {b, B}, {a, Alpha}]);
shapeJSON(packman, X, Y, [R, G, B], Alpha) ->
    %Radius = random:uniform(random:uniform(10)),
    Radius = 10,
    jsx:encode([{shape, ellipse},
                {x, X}, {y, Y},
                {rad, Radius},
                {start, math:pi() * 0.25}, {'end', math:pi() * 1.75},
                {r, R}, {g, G}, {b, B}, {a, Alpha}]).

rand_color_elem(0) ->
    0;
rand_color_elem(X) ->
    random:uniform(X).
