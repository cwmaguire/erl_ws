-module(erl_ws_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [{animate, {animate, start_gen_server, []}, permanent, brutal_kill, worker, [animate]}],
	{ok, {{simple_one_for_one, 1, 5}, Procs}}.
