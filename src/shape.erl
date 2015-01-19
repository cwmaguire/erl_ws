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
