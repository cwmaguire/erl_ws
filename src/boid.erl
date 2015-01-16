-module(boid).

-export([boid/5]).

boid(Pid, Shape, Height, Width, RGBMax) ->
    _ = random:seed(os:timestamp()),
    {X, Y} = {random:uniform(Height), random:uniform(Width)},
    RGB = [rand_color_elem(Elem) || Elem <- RGBMax],
    Alpha = (random:uniform(9) + 1) / 10,

    JSON = shapeJSON(Shape, X, Y, RGB, Alpha),

    io:format("boid ~p sending ~p~n", [self(), JSON]),
    Pid ! JSON,
    erlang:send_after(100, self(), draw),
    receive
        draw ->
            %io:format("boid ~p received draw~n", [self()]),
            boid(Pid, Shape, Height, Width, RGBMax);
        Other ->
            io:format("boid ~p received ~p~n", [self(), Other]),
            ok
    after 5000 ->
        io:format("boid ~p didn't receive anything~n", [self()]),
        ok
    end.

shapeJSON(rectangle, X, Y, [R, G, B], Alpha) ->
    {H, W} = {random:uniform(50), random:uniform(50)},
    jsx:encode([{shape, rectangle},
                {x, X}, {y, Y},
                {h, H}, {w, W},
                {r, R}, {g, G}, {b, B}, {a, Alpha}]);
shapeJSON(ellipse, X, Y, [R, G, B], Alpha) ->
    Radius = random:uniform(random:uniform(50)),
    jsx:encode([{shape, ellipse},
                {x, X}, {y, Y},
                {rad, Radius},
                {start, 0}, {'end', 2 * math:pi()},
                {r, R}, {g, G}, {b, B}, {a, Alpha}]);
shapeJSON(packman, X, Y, [R, G, B], Alpha) ->
    Radius = random:uniform(random:uniform(50)),
    jsx:encode([{shape, ellipse},
                {x, X}, {y, Y},
                {rad, Radius},
                {start, math:pi() * 0.25}, {'end', math:pi() * 1.75},
                {r, R}, {g, G}, {b, B}, {a, Alpha}]).

rand_color_elem(0) ->
    0;
rand_color_elem(X) ->
    random:uniform(X).
