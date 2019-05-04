-module(demo).

-export([start/0]).

start() ->
    {ok, File} = file:open("Newfile.txt", [read]),
    Txt = file:read(File, 1024 * 1024),
    start_1(1, 2),
    start_2(1, 2),
    io:fwrite("~p~n", [Txt]).

start_1(a, b) -> a + b.

start_2(a, b) -> a + b.
