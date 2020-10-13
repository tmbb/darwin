defmodule Darwin.DefaultMutators.AbstractCodeTests.LaxLogicalOperatorsTest do
  use ExUnit.Case, async: true
  import DarwinTest.Helpers
  alias Darwin.Erlang
  alias Darwin.Mutator.Context

  doctest Darwin

  @tag :skip
  test "operator: &&" do
    {abstract_code, ctx} = mutate_elixir("a && b")
    # Assert that we generate the correct erlang code.
    assert Erlang.assert_equivalent(abstract_code, """
           'Elixir.Darwin.Mutators.Default.OpLaxAndMutator':darwin_was_here('Elixir.MyModule',
                                                                       0, _a@1, _b@1).
           """)

    # Assert the correct number of mutations is generated.
    assert Context.nr_of_mutations(ctx) == 3
  end

  @tag :skip
  test "operator: ||" do
    {abstract_code, ctx} = mutate_elixir("a || b")
    # Assert that we generate the correct erlang code.
    assert Erlang.assert_equivalent(abstract_code, """
           'Elixir.Darwin.Mutators.Default.OpLaxOrMutator':darwin_was_here('Elixir.MyModule',
                                                                      0, _a@1, _b@1).
           """)

    # Assert the correct number of mutations is generated.
    assert Context.nr_of_mutations(ctx) == 3
  end

  @tag :skip
  test "operator: !" do
    {abstract_code, ctx} = mutate_elixir("!a")
    # Assert that we generate the correct erlang code.
    assert Erlang.assert_equivalent(abstract_code, """
           'Elixir.Darwin.Mutators.Default.OpLaxNotMutator':darwin_was_here('Elixir.MyModule',
                                                                      0, _a@1).
           """)

    # Assert the correct number of mutations is generated.
    assert Context.nr_of_mutations(ctx) == 3
  end
end
