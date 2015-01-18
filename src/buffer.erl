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
