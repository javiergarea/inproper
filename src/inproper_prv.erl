-module(inproper_prv).

-export([init/1, do/1, format_error/1]).

-include_lib("kernel/include/file.hrl").

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
                          {opts, []},                   % list of options understood by the plugin
                          {short_desc,
                           "Induction of PropEr properties based on sample unit tests."},
                          {desc, "Induction of PropEr properties based on sample unit tests."}]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Running InPropEr!", []),
    lists:foreach(fun inproper_run/1, rebar_state:project_apps(State)),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private
%% ===================================================================
-spec inproper_run(rebar_state:t()) -> ok.
inproper_run(App) ->
    TestDir = filename:join([rebar_app_info:dir(App), "test"]),
    rebar_api:debug("Finding tests in: ~p~n", [TestDir]),
    Paths = find_tests(TestDir),
    rebar_api:debug("Found tests in: ~p~n", [Paths]),
    ok.

-spec find_tests(file:name()) -> [file:filename_all()].
find_tests(Dir) ->
    case file:list_dir(Dir) of
        {ok, Filenames} ->
            Files = [filename:join([Dir, Filename]) || Filename <- Filenames],
            lists:flatmap(fun(File) ->
                             case get_file_type(File) of
                                 directory -> find_tests(File);
                                 _ -> [File]
                             end
                          end,
                          Files);
        {error, Reason} ->
            format_error(Reason),
            exit(Reason)
    end.

-spec get_file_type(file:filename_all()) ->
                       device | directory | other | regular | symlink | undefined.
get_file_type(File) ->
    rebar_api:debug("Looking for the type of file: ~p~n", [File]),
    case file:read_file_info(File) of
        {ok, FileInfo} ->
            FileInfo#file_info.type;
        {error, Reason} ->
            format_error(Reason),
            exit(Reason)
    end.
