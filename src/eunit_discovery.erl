-module(eunit_discovery).

-export([find_tests/1]).

-include_lib("kernel/include/file.hrl").

-behaviour(test_discovery).

-spec find_tests(file:name()) ->
                    {ok, [file:filename_all()]} | {error, file:posix() | badarg}.
find_tests(Dir) ->
    case file:list_dir(Dir) of
        {ok, Filenames} ->
            Files = [filename:join([Dir, Filename]) || Filename <- Filenames],
            lists:flatmap(fun(File) ->
                             case get_file_type(File) of
                                 {ok, directory} -> find_tests(File);
                                 {ok, regular} -> [File];
                                 {error, Reason} -> {error, Reason};
                                 _ -> ok
                             end
                          end,
                          Files);
        {error, Reason} ->
            {error, Reason}
    end.

get_file_type(File) ->
    rebar_api:debug("Looking for the type of file: ~p~n", [File]),
    case file:read_file_info(File) of
        {ok, FileInfo} ->
            FileType = FileInfo#file_info.type,
            rebar_api:debug("Type found: ~p~n", [FileType]),
            {ok, FileType};
        {error, Reason} ->
            {error, Reason}
    end.
