-module(eunit_discovery).

-export([find_testfiles/1]).

-include_lib("kernel/include/file.hrl").

-behaviour(test_discovery).

-define(EUNIT_HEADER, "-include_lib(\"eunit/include/eunit.hrl\").").

-spec find_testfiles(Dir :: file:name()) ->
                        {ok, [file:filename_all()]} | {error, file:posix() | badarg}.
find_testfiles(Dir) ->
    case rec_list_dir(Dir) of
        {error, Reason} ->
            {error, Reason};
        RecFiles ->
            ErlFiles =
                lists:filter(fun(File) ->
                                case string:find(File, ".erl", trailing) of
                                    nomatch -> false;
                                    _ -> true
                                end
                             end,
                             RecFiles),
            TestFiles =
                lists:filter(fun(File) ->
                                case file:read_file(File) of
                                    {ok, Binary} ->
                                        rebar_api:debug("Succesfully read ~p~n", [File]),
                                        case binary:match(Binary, <<?EUNIT_HEADER>>) of
                                            nomatch -> false;
                                            _ -> true
                                        end;
                                    {error, Reason} ->
                                        rebar_api:debug("Error reading ~p: ~p~n", [File, Reason]),
                                        false
                                end
                             end,
                             ErlFiles),
            {ok, TestFiles}
    end.

rec_list_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Filenames} ->
            Files = [filename:join([Dir, Filename]) || Filename <- Filenames],
            RecFiles =
                lists:flatmap(fun(File) ->
                                 case get_file_type(File) of
                                     directory -> rec_list_dir(File);
                                     regular -> [File];
                                     _ -> []
                                 end
                              end,
                              Files),
            RecFiles;
        {error, Reason} ->
            {error, Reason}
    end.

get_file_type(File) ->
    rebar_api:debug("Looking for the type of file: ~p~n", [File]),
    case file:read_file_info(File) of
        {ok, FileInfo} ->
            FileType = FileInfo#file_info.type,
            rebar_api:debug("Type found: ~p~n", [FileType]),
            FileType;
        {error, _Reason} ->
            error
    end.
