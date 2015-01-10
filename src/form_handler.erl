-module(form_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

init(_, Req, _Opts) ->
    io:format("Form handler init~n"),
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    HasBody = cowboy_req:has_body(Req),
    io:format("Has body? ~p~n", [atom_to_list(HasBody)]),

    {Headers, Req2} = cowboy_req:headers(Req),
    io:format("Headers:~n\t~p~n", [Headers]),
    BinHeaders = lists:flatten([["<br>Key: ",K,"; Val: ", V] || {K, V} <- Headers]),

    {Host, Req3} = cowboy_req:host(Req2),
    {HostURL, Req4} = cowboy_req:host_url(Req3),

    {ok, Req5} = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/html">>}],
        ["<html><body>Has body: ",
         atom_to_list(HasBody), "<br>",
         BinHeaders, "<br>",
         "Host: ", Host, "<br>",
         "Host URL: ", HostURL, "<br>",
         "</body></html>"],
        Req4),
    {ok, Req5, State}.

terminate(_Reason, _Req, _State) ->
	ok.
