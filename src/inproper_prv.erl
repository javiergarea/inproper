-module(inproper_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, inproper).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 inproper"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "Induction of PropEr properties based on sample unit tests."},
            {desc, "Induction of PropEr properties based on sample unit tests."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    lists:foreach(fun inproper_run/1, rebar_state:project_apps(State)),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).


%% ===================================================================
%% Private
%% ===================================================================
-spec inproper_run(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
inproper_run(App) ->
    Path = find_tests(App),
    ok.

%% -spec find_tests()
find_tests(App) ->
    App.
