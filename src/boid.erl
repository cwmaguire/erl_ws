-module(boid).

-export([boid/4]).

boid(Pid, Height, Width, RGB) ->
    fun() ->
        _ = random:seed(os:timestamp()),
        {X, Y} = {random:uniform(Height), random:uniform(Width)},
        [R, G, B] = [rand_color_elem(Elem) || Elem <- RGB],
        Alpha = (random:uniform(9) + 1) / 10,
        JSON = io_lib:format("{\"type\":\"square\",\"x\":~b,\"y\":~b,\"r\":~b,\"g\":~b,\"b\":~b,\"a\":~f}",
                             [X, Y, R, G, B, Alpha]),
        io:format("boid ~p sending ~p~n", [self(), lists:flatten(JSON)]),
        Pid ! JSON,
        erlang:send_after(100, self(), draw),
        receive
            draw ->
                io:format("boid ~p received draw~n", [self()]),
                (boid(Pid, Height, Width, RGB))();
            Other ->
                io:format("boid ~p received ~p~n", [self(), Other]),
                ok
        after 5000 ->
            io:format("boid ~p didn't receive anything~n", [self()]),
            ok
        end
    end.

rand_color_elem(0) ->
    0;
rand_color_elem(X) ->
    random:uniform(X).
