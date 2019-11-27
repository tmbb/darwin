defmodule Darwin.DefaultMutators.AbstractCodeTests.LaxLogicalOperatorsTest do
  use ExUnit.Case, async: true
  import DarwinTest.Helpers
  alias Darwin.Erlang
  alias Darwin.Mutator.Context

  doctest Darwin

  test "operator: &&" do
    {abstract_code, ctx} = mutate_elixir("a && b")
    # Assert that we generate the correct erlang code.
    assert Erlang.equivalent?(abstract_code, """
           'Elixir.Darwin.Mutators.Default.OpLaxAndMutator':do_mutate('Elixir.MyModule',
                                                                       0, _a@1, _b@1).
           """)

    # Assert the correct number of mutations is generated.
    assert Context.nr_of_mutations(ctx) == 3
  end

  test "operator: ||" do
    {abstract_code, ctx} = mutate_elixir("a || b")
    # Assert that we generate the correct erlang code.
    assert Erlang.equivalent?(abstract_code, """
           'Elixir.Darwin.Mutators.Default.OpLaxOrMutator':do_mutate('Elixir.MyModule',
                                                                      0, _a@1, _b@1).
           """)

    # Assert the correct number of mutations is generated.
    assert Context.nr_of_mutations(ctx) == 3
  end

  test "operator: !" do
    {abstract_code, ctx} = mutate_elixir("!a")
    # Assert that we generate the correct erlang code.
    assert Erlang.equivalent?(abstract_code, """
           'Elixir.Darwin.Mutators.Default.OpLaxNotMutator':do_mutate('Elixir.MyModule',
                                                                      0, _a@1).
           """)

    # Assert the correct number of mutations is generated.
    assert Context.nr_of_mutations(ctx) == 3
  end
end
