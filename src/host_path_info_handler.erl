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

%% host and path info reference
%%
%% Demonstrate getting the [...] porting of the routing rule binding
%% from the request
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
    {PathInfo, Req3} = cowboy_req:path_info(Req2),
    io:format("Found:~n\tHost Info: ~p~n\tPath Info: ~p~n", [HostInfo, PathInfo]),
    {ok, Req4} = cowboy_req:reply(200,
                                  [{<<"content-type">>, <<"text/plain">>}],
                                  [<<"Host Info: ">>, HostInfo, <<"<br>Path Info: ">>, PathInfo],
                                  Req3),
    {ok, Req4, State}.

% Used to clean up any resources, messages, timers, etc.
terminate(_Reason, _Req, _State) ->
	ok. % can't return anything else
