-module(test_discovery).

-callback find_tests(file:name()) ->
                        {ok, [file:filename_all()]} | {error, file:posix() | badarg}.
