-module(erl_ws_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [{animate, {gen_server, start_link, [{local, animate}, animate, [], []]}, permanent, brutal_kill, worker, [animate]}],
	{ok, {{one_for_one, 1, 5}, Procs}}.
