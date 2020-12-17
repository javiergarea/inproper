-module(inproper_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, inproper).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create([{name, ?PROVIDER},            % The 'user friendly' name of the task
                          {module, ?MODULE},            % The module implementation of the task
                          {bare,
                           true},                 % The task can be run by the user, always true
                          {deps, ?DEPS},                % The list of dependencies
                          {example, "rebar3 inproper"}, % How to use the plugin
                          {opts,
                           [{framework,
                             $f,
                             "framework",
                             "eunit",
                             "Test framework used for writing the base tests."},
                            {dir,
                             $d,
                             "dir",
                             [],
                             "Comma separated list of dirs to load tests from."}]},                   % list of options understood by the plugin
                          {short_desc,
                           "Induction of PropEr properties based on sample unit tests."},
                          {desc, "Induction of PropEr properties based on sample unit tests."}]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    inproper_run(State),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private
%% ===================================================================
inproper_run(State) ->
    rebar_api:info("Running InPropEr!", []),
    {TestFramework, Dirs} = parse_args(State),
    case TestFramework of
        _ ->
            TestDiscovery = eunit_discovery
    end,
    lists:foreach(fun(X) -> inproper_app_run(X, TestDiscovery, Dirs) end,
                  rebar_state:project_apps(State)),
    ok.

parse_args(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    rebar_api:debug("Parsed args: ~p~n", [Args]),
    TestFramework = get_test_framework(Args),
    Dirs = get_test_dirs(Args),
    rebar_api:debug("Chosen test framework: ~p~n", [TestFramework]),
    rebar_api:debug("Chosen test dirs: ~p~n", [Dirs]),
    {TestFramework, Dirs}.

get_test_framework(Args) ->
    case proplists:get_value(framework, Args) of
        _ ->
            eunit
    end.

get_test_dirs(Args) ->
    case proplists:get_value(dir, Args) of
        undefined ->
            [];
        X ->
            X
    end.

inproper_app_run(App, TestDiscovery, Dirs) ->
    case Dirs of
        [] ->
            TestDir = filename:join([rebar_app_info:dir(App), "test"]),
            rebar_api:debug("Finding tests in: ~p~n", [TestDir]),
            Paths = TestDiscovery:find_tests(TestDir);
        _ ->
            Paths = Dirs
    end,
    rebar_api:debug("Found tests in: ~p~n", [Paths]).
