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

%% Demonstrate chunked body handling
-module(chunked_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

init(_, Req, _Opts) ->
    io:format("Chunked handler init~n"),
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    HasBody = cowboy_req:has_body(Req),
    io:format("Has body? ~p~n", [atom_to_list(HasBody)]),

    {BodyChunks, Req2} = body(Req),

    {ok, Req3} = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/html">>}],
        ["<html><body>",
         "Has body: ", atom_to_list(HasBody), "<br>",
         "Body: <br>", BodyChunks, "<br>",
         "</body></html>"],
        Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
	ok.

%% Chunk length doesn't specify the exact chunk size, it just limits the
%% Cowboy buffer so that if the buffer gets as much or more than you specify
%% Cowboy will send as much as it has. If the Cowboy accumulator has less
%% than you've specified then it will grab it's next chunk (the size of which
%% is determined by Cowboy).
body(Req) ->
    body(cowboy_req:body(Req, [{length, 1}]), ["Chunk: "]).

body({ok, Chunk, Req}, Chunks) ->
    io:format(standard_error, "Final body chunk:~n\t~p~n", [Chunk]),
    {lists:reverse([Chunk | Chunks]), Req};
body({more, Chunk, Req}, Chunks) ->
    io:format(standard_error, "Got body chunk:~n\t~p~n", [Chunk]),
    body(cowboy_req:body(Req, [{length, 10}]), ["<br>Chunk: ", Chunk | Chunks]).
