-module(buffer).

-export([start/2]).

start(OutPid, Wait) ->
    ets:new(buffer, [public, named_table, duplicate_bag]),
    erlang:send_after(Wait, self(), send),
    buffer(OutPid, Wait).

buffer(OutPid, Wait) ->
    receive
        send ->
            io:format("buffer ~p sending.~n", [self()]),
            send(OutPid),
            erlang:send_after(Wait, self(), send),
            buffer(OutPid, Wait);
        stop ->
            ets:delete(buffer);
        Value ->
            io:format("buffer ~p receiving ~p~n", [self(), Value]),
            ets:insert(buffer, {value, Value}),
            buffer(OutPid, Wait)
    end.

send(OutPid) ->
    case ets:tab2list(buffer) of
        [] ->
            ok;
        Recs ->
            [[_, FirstValue] | Values] = [[<<$,>>, Value] || {_, Value} <- Recs],
            WrappedValues = [$[, FirstValue, Values, $]],
            ets:delete_all_objects(buffer),
            OutPid ! WrappedValues
    end.
