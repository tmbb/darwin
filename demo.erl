-module(demo).

-export([start/0]).

start() ->
    {ok, File} = file:open("Newfile.txt", [read]),
    Txt = file:read(File, 1024 * 1024),
    A = 1,
    B = 2,
    C = 3,
    start_1(1, 2),
    start_4(A, start_3(B, C)),
    io:fwrite("~p~n", [Txt]).

start_3(_darwin_a@1, _darwin_b@1) ->
    case
      'Elixir.Darwin.Mutator.Helpers':do_get_active_mutation()
	of
      1 ->
	  'Elixir.Darwin.Mutator.Helpers':do_arith_add(_darwin_a@1,
						       _darwin_b@1);
      2 ->
	  'Elixir.Darwin.Mutator.Helpers':do_arith_sub(_darwin_a@1,
						       _darwin_b@1);
      3 ->
	  'Elixir.Darwin.Mutator.Helpers':do_arith_div(_darwin_a@1,
						       _darwin_b@1);
      _ ->
	  'Elixir.Darwin.Mutator.Helpers':do_arith_mul(_darwin_a@1,
						       _darwin_b@1)
    end.

start_4(_darwin_a@1, _darwin_b@1) ->
    case
      'Elixir.Darwin.Mutator.Helpers':do_get_active_mutation()
	of
      4 ->
	  'Elixir.Darwin.Mutator.Helpers':do_arith_sub(_darwin_a@1,
						       _darwin_b@1);
      5 ->
	  'Elixir.Darwin.Mutator.Helpers':do_arith_mul(_darwin_a@1,
						       _darwin_b@1);
      6 ->
	  'Elixir.Darwin.Mutator.Helpers':do_arith_div(_darwin_a@1,
						       _darwin_b@1);
      _ ->
	  'Elixir.Darwin.Mutator.Helpers':do_arith_add(_darwin_a@1,
						       _darwin_b@1)
    end.

start_1(a, b) -> a + b.
