-file("src/test.erl", 1).

-module(test).

-compile([no_auto_import]).

-export([f/1]).

f(X) -> X.
