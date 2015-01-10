-module(host_path_info_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

init(_, Req, _Opts) ->
    io:format("host_path_info init~n"),
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    {HostInfo, Req2} = cowboy_req:host_info(Req),
    {PathInfo, Req3} = cowboy_req:host_info(Req2),
    io:format("Found:~n\tHost Info: ~p~n\tPath Info: ~p~n", [HostInfo, PathInfo]),
    {ok, Req4} = cowboy_req:reply(200,
                                  [{<<"content-type">>, <<"text/plain">>}],
                                  [<<"Host Info: ">>, HostInfo, <<" Path Info: ">>, PathInfo],
                                  Req3),
    {ok, Req4, State}.

% Used to clean up any resources, messages, timers, etc.
terminate(_Reason, _Req, _State) ->
	ok. % can't return anything else
