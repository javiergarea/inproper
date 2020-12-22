-module(inproper_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, inproper).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(State :: rebar_state:t()) -> {ok, rebar_state:t()}.
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
                             undefined,
                             "Test framework used for writing the base tests."},
                            {dir,
                             $d,
                             "dir",
                             undefined,
                             "Comma separated list of dirs to load tests from."}]},                   % list of options understood by the plugin
                          {short_desc,
                           "Induction of PropEr properties based on sample unit tests."},
                          {desc, "Induction of PropEr properties based on sample unit tests."}]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(State :: rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    inproper_run(State),
    {ok, State}.

-spec format_error(Reason :: any()) -> iolist().
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
    TestFramework = parse_test_framework(Args),
    TestDirs = parse_test_dirs(Args),
    rebar_api:debug("Chosen test framework: ~p~n", [TestFramework]),
    rebar_api:debug("Chosen test dirs: ~p~n", [TestDirs]),
    {TestFramework, TestDirs}.

parse_test_framework(Args) ->
    case proplists:get_value(framework, Args) of
        undefined ->
            eunit;
        Framework ->
            Framework
    end.

parse_test_dirs(Args) ->
    case proplists:get_value(dir, Args) of
        undefined ->
            [];
        Dirs ->
            Dirs
    end.

inproper_app_run(App, TestDiscovery, Dirs) ->
    Paths = get_test_paths(App, TestDiscovery, Dirs),
    rebar_api:debug("Found tests in: ~p~n", [Paths]),
    get_test_info(TestDiscovery, Paths).

get_test_paths(App, TestDiscovery, Dirs) ->
    case Dirs of
        [] ->
            TestDir = filename:join([rebar_app_info:dir(App), "test"]),
            rebar_api:debug("Finding tests in: ~p~n", [TestDir]),
            case TestDiscovery:find_testfiles(TestDir) of
                {ok, Paths} ->
                    Paths;
                {error, Reason} ->
                    format_error(Reason),
                    exit(Reason)
            end;
        _ ->
            Dirs
    end.

get_test_info(TestDiscovery, Paths) ->
    case Paths of
        [] ->
            ok;
        _ ->
            lists:foreach(fun(File) -> TestDiscovery:read_tests(File) end, Paths)
    end,
    ok.
