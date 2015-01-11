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

%% Demonstrate Cowboy routing:
%% - root path
%% - static files
%% - Host and path [...]
%%
%% Demonstrate getting the [...] porting of the routing rule binding
%% from the request
-module(erl_ws_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-define(ANY_HOST, '_').
-define(NO_OPTIONS, []).

%orig
%start(_Type, _Args) ->
	%erl_ws_sup:start_link().

%cowboy
start(_Type, _Args) ->
    % Bindings can be per host or per path
    %
    % HostWithConstraints = {Host, ConstraintExamples, Paths},
    % PathWithConstraints = {Path, ConstraintExamples, Handler, Bindings},
    %
    % Can also compile and update routes on the fly:
    %    cowboy:set_env(my_http_listener, dispatch, cowboy_router:compile(Dispatch)).

    % constraint function arguments are BINARY!
    Constraints = [{an_int, int},
                   {three_chars, function, fun three_chars/1},
                   {add_one, function, fun add_one/1}%,
                   %{non_empty, nonempty}, %% not_implemented in this version
                  ],

    Paths = [{"/hello", hello_handler, _Bindings = []},
             {"/", cowboy_static, {priv_file, erl_ws, "static/form.html"}},
             {"/form", form_handler, ?NO_OPTIONS},
             {"/chunked_form", chunked_handler, ?NO_OPTIONS},
             {"/constraints/:anything", constraints_handler, {constraints_met, true}},
             {"/constraints/:an_int/:three_chars/[:add_one]", Constraints, constraints_handler, {constraints_met, true}},
             {"/constraints/[...]", constraints_handler, {constraints_met, false}},
             {"/[...]", cowboy_static, {priv_dir, erl_ws, "static"}}],

    Routes = [{?ANY_HOST, Paths},
              {"[...]", [{"/host_path_info/[...]", host_path_info_handler, ?NO_OPTIONS}]}],
    %[...] in the host spec must be in a string
    %[...] in the path spec must follow a slash

    Dispatch = cowboy_router:compile(Routes),
    cowboy:start_http(my_http_listener, 100, [{port, 8080}], [{env, [{dispatch, Dispatch}]}]),
    erl_ws_sup:start_link().

stop(_State) ->
	ok.

three_chars(<<X:8, Y:8, Z:8>> = Chars) ->
    io:format(standard_error, "three_chars(~p) pass~n", [[X, Y, Z]]),
    AllChars = lists:all(fun is_char/1, binary_to_list(Chars)),
    io:format(standard_error, "All chars? ~p~n", [AllChars]),
    AllChars;
three_chars(Other) ->
    io:format(standard_error, "three_chars(~p) fail~n", [Other]),
    false.

is_char(X) when is_integer(X), X >= 32, X =< 126 ->
    true;
is_char(_) ->
    false.

add_one(<<X:8>>) when is_integer(X) ->
    io:format(standard_error, "add_one(~p) pass~n", [X]),
    {true, list_to_integer([X]) + 1};
add_one(Other) ->
    io:format(standard_error, "add_one(~p) fail~n", [Other]),
    false.
