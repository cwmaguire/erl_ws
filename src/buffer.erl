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
-module(buffer).

-export([start/2]).

start(OutPid, Wait) ->
    erlang:send_after(Wait, self(), send),
    buffer(OutPid, Wait, dict:new()).

buffer(OutPid, Wait, Buffer) ->
    receive
        send ->
            send(OutPid, Buffer),
            erlang:send_after(Wait, self(), send),
            buffer(OutPid, Wait, Buffer);
        stop ->
            ok;
        {Pid, Value} ->
            buffer(OutPid, Wait, dict:update(Pid, fun(_) -> Value end, Value, Buffer))
    end.

send(OutPid, Buffer) ->
    Values = [Value || {_, Value} <- dict:to_list(Buffer)],
    JSON = jsx:encode([{canvas, <<"boids">>}, {objs, Values}]),
    OutPid ! JSON.
