-module(erl_ws_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

%orig
%start(_Type, _Args) ->
	%erl_ws_sup:start_link().

%cowboy
start(_Type, _Args) ->
    AnyHost = '_',

    % Constraint examples to remind me that it's possible to:
    %     - auto-convert to ints,
    %     - validate whether the URL has correct bindings
    %     - validate bindings and change (or augment) the binding value
    %
    % Bindings can be per host or per path
    %
    % ConstraintExamples = [{id, int},
    %                       {type, function, fun(player) -> true end},
    %                       {type, function, fun(enemy) -> {true, {some_value, some_other_value}} end}],
    % HostWithConstraints = {Host, ConstraintExamples, Paths},
    % PathWithConstraints = {Path, ConstraintExamples, Handler, Bindings},
    %
    % Can also compile and update routes on the fly:
    %    cowboy:set_env(my_http_listener, dispatch, cowboy_router:compile(Dispatch)).

    Paths = [{"/", hello_handler, _Bindings = []},
             {"/form", cowboy_static, {priv_file, erl_ws, "static/form.html"}},
             {"/simple_form", form_handler, _Bindings = []}],
    Routes = [{AnyHost, Paths}],
    %Routes = [{"[...]", [{"/host_path_info/[...]", host_path_info_handler, _Bindings = []}]}],
    %[...] in the host spec must be in a string
    %[...] in the path spec must follow a slash
    Dispatch = cowboy_router:compile(Routes),
    %Dispatch = cowboy_router:compile([{'_', [{"/", hello_handler, []}]} ]),
    cowboy:start_http(my_http_listener, 100, [{port, 8080}], [{env, [{dispatch, Dispatch}]}]),
    %hello_erlang_sup:start_link().
    erl_ws_sup:start_link().

stop(_State) ->
	ok.
