-module(shape).

-export([shape/4]).
-export([shape/6]).

shape(Shape, Point, Size, Colour) ->
    shape(Shape, Point, Size, Colour, _FillStyle = color, _GradientColour = {0, 0, 0, 0.0}).

shape(rectangle, {X, Y}, Size, {R, G, B, A}, FillStyle, {GR, GG, GB, GA}) ->
    [{shape, rectangle},
     {x, X}, {y, Y},
     {h, Size}, {w, Size},
     {r, R}, {g, G}, {b, B}, {a, A},
     {fill, FillStyle},
     {gr, GR}, {gg, GG}, {gb, GB}, {ga, GA}];
shape(ellipse, {X, Y}, Size, {R, G, B, A}, FillStyle, {GR, GG, GB, GA}) ->
    [{shape, ellipse},
     {x, X}, {y, Y},
     {rad, Size},
     {start, 0}, {'end', 2 * math:pi()},
     {r, R}, {g, G}, {b, B}, {a, A},
     {fill, FillStyle},
     {gr, GR}, {gg, GG}, {gb, GB}, {ga, GA}];
shape(packman, {X, Y}, Size, {R, G, B, A}, FillStyle, {GR, GG, GB, GA}) ->
    [{shape, ellipse},
     {x, X}, {y, Y},
     {rad, Size},
     {start, math:pi() * 0.25}, {'end', math:pi() * 1.75},
     {r, R}, {g, G}, {b, B}, {a, A},
     {fill, FillStyle},
     {gr, GR}, {gg, GG}, {gb, GB}, {ga, GA}].
