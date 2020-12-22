-module(test_discovery).

-callback find_testfiles(file:name()) ->
                            {ok, [file:filename_all()]} | {error, file:posix() | badarg}.
