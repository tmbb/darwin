defmodule DarwinTest do
  use ExUnit.Case
  alias Darwin.ErlUtils
  doctest Darwin

  def mutate(bin) do
    bin
    |> ErlUtils.expression!()
    |> Darwin.mutate()
    |> ErlUtils.pprint()
  end

  # Arithmetic operator replacement

  test "operator: +" do
    assert mutate("A + B.") == """
           fun (_darwin_a@1, _darwin_b@1) ->
                   case
                     'Elixir.Darwin.Mutator.Helpers':do_get_active_mutation()
                       of
                     1 ->
                         'Elixir.Darwin.Mutator.Helpers':do_arith_sub(_darwin_a@1,
                                                                      _darwin_b@1);
                     2 ->
                         'Elixir.Darwin.Mutator.Helpers':do_arith_mul(_darwin_a@1,
                                                                      _darwin_b@1);
                     3 ->
                         'Elixir.Darwin.Mutator.Helpers':do_arith_div(_darwin_a@1,
                                                                      _darwin_b@1);
                     _ ->
                         'Elixir.Darwin.Mutator.Helpers':do_arith_add(_darwin_a@1,
                                                                      _darwin_b@1)
                   end
           end(A, B)
           """
  end

  test "operator: -" do
    assert mutate("A - B.") == """
           fun (_darwin_a@1, _darwin_b@1) ->
                   case
                     'Elixir.Darwin.Mutator.Helpers':do_get_active_mutation()
                       of
                     1 ->
                         'Elixir.Darwin.Mutator.Helpers':do_arith_add(_darwin_a@1,
                                                                      _darwin_b@1);
                     2 ->
                         'Elixir.Darwin.Mutator.Helpers':do_arith_mul(_darwin_a@1,
                                                                      _darwin_b@1);
                     3 ->
                         'Elixir.Darwin.Mutator.Helpers':do_arith_div(_darwin_a@1,
                                                                      _darwin_b@1);
                     _ ->
                         'Elixir.Darwin.Mutator.Helpers':do_arith_sub(_darwin_a@1,
                                                                      _darwin_b@1)
                   end
           end(A, B)
           """
  end

  test "operator: *" do
    assert mutate("A * B.") == """
           fun (_darwin_a@1, _darwin_b@1) ->
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
                   end
           end(A, B)
           """
  end

  test "operator: /" do
    assert mutate("A / B.") == """
           fun (_darwin_a@1, _darwin_b@1) ->
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
                         'Elixir.Darwin.Mutator.Helpers':do_arith_mul(_darwin_a@1,
                                                                      _darwin_b@1);
                     _ ->
                         'Elixir.Darwin.Mutator.Helpers':do_arith_div(_darwin_a@1,
                                                                      _darwin_b@1)
                   end
           end(A, B)
           """
  end

  # Relational operator replacement
end
