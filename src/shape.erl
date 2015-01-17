-module(shape).

-export([shape/4]).

shape(rectangle, {X, Y}, Size, {R, G, B, A}) ->
    [{shape, rectangle},
     {x, X}, {y, Y},
     {h, Size}, {w, Size},
     {r, R}, {g, G}, {b, B}, {a, A}];
shape(ellipse, {X, Y}, Size, {R, G, B, A}) ->
    [{shape, ellipse},
     {x, X}, {y, Y},
     {rad, Size},
     {start, 0}, {'end', 2 * math:pi()},
     {r, R}, {g, G}, {b, B}, {a, A}];
shape(packman, {X, Y}, Size, {R, G, B, A}) ->
    [{shape, ellipse},
     {x, X}, {y, Y},
     {rad, Size},
     {start, math:pi() * 0.25}, {'end', math:pi() * 1.75},
     {r, R}, {g, G}, {b, B}, {a, A}].
