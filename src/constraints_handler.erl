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

%% Demonstrate URL constraints
%%
%% Ensure binding is an int
%% Run binding through boolean function
%% Run binding through update function
-module(constraints_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {are_constraints_met = nil :: boolean()}).

init(_, Req, _Opts = {constraints_met, AreConstraintsMet}) ->
    io:format("Constraints met: ~p~n", [atom_to_list(AreConstraintsMet)]),
	{ok, Req, #state{are_constraints_met = AreConstraintsMet}}.

handle(Req, State = #state{are_constraints_met = AreConstraintsMet}) ->
    {Anything, Req2} = cowboy_req:binding(anything, Req, "nothing"),
    {Int, Req2} = cowboy_req:binding(an_int, Req, -1),
    {Chars, Req3} = cowboy_req:binding(three_chars, Req2, "not three chars"),
    {IncInt, Req4} = cowboy_req:binding(add_one, Req3, -1),
    {PathInfo, Req5} = cowboy_req:path_info(Req4),

    io:format("Bindings:~n\tan_int: ~p~n\tthree_chars: ~p~n\tadd_one: ~p~n", [Int, Chars, IncInt]),
    io:format("Path Info: ~p~n", [PathInfo]),

    SafePathInfo = case PathInfo of
                       undefined -> "undefined";
                       _ -> PathInfo
                   end,

    {ok, Req6} = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/html">>}],
        ["<html><body>",
         "Constraints met? ", atom_to_list(AreConstraintsMet), "<br>",
         "Anything binding (int): ", Anything, "<br>",
         "First binding (int): ", integer_to_list(Int), "<br>",
         "Second binding (3 chars): ", Chars, "<br>",
         "Third binding (incremented int): ", integer_to_list(IncInt), "<br>",
         "Path Info: ", SafePathInfo, "<br>",
         "</body></html>"],
        Req5),
    {ok, Req6, State}.

terminate(_Reason, _Req, _State) ->
	ok.
