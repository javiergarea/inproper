-module(eunit_discovery).

-export([find_testfiles/1, read_tests/1]).

-include_lib("kernel/include/file.hrl").

-behaviour(test_discovery).

-define(EUNIT_HEADER, "-include_lib(\"eunit/include/eunit.hrl\").").

%% ===================================================================
%% Public API
%% ===================================================================
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

-spec read_tests(file:filename_all()) -> ok.
read_tests(File) ->
    case file:read_file(File) of
        {ok, Binary} ->
            case erl_scan:string(
                     binary:bin_to_list(Binary))
            of
                {ok, Tokens, _Location} ->
                    rebar_api:debug("Scanning ~p: ~p~n", [File, Tokens]),
                    case erl_parse:parse_exprs(Tokens) of
                        {ok, ExprList} ->
                            rebar_api:debug("Parsing ~p: ~p~n", [File, ExprList]);
                        {error, Reason} ->
                            rebar_api:debug("Error parsing ~p: ~p~n", [File, Reason])
                    end;
                {error, Reason, _Location} ->
                    rebar_api:debug("Error scanning ~p: ~p~n", [File, Reason])
            end;
        {error, Reason} ->
            rebar_api:debug("Error reading ~p: ~p~n", [File, Reason])
    end,
    ok.

%% ===================================================================
%% Private
%% ===================================================================
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
