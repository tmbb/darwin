defmodule Darwin.DefaultMutators.AbstractCodeTests.StrictLogicalOperatorsTest do
  use ExUnit.Case, async: true
  import Darwin.TestHelpers
  alias Darwin.Erlang
  alias Darwin.Mutator.Context

  doctest Darwin

  test "operator: and" do
    {abstract_code, ctx} = mutate_elixir("a and b")
    # Assert that we generate the correct erlang code.
    assert Erlang.assert_equivalent(abstract_code, """
           'Elixir.Darwin.Mutators.Default.OpStrictAndMutator':darwin_was_here('Elixir.MyModule',
                                                                         0, _a@1, _b@1).
           """)

    # Assert the correct number of mutations is generated.
    assert Context.nr_of_mutations(ctx) == 3
  end

  test "operator: or" do
    {abstract_code, ctx} = mutate_elixir("a or b")
    # Assert that we generate the correct erlang code.
    assert Erlang.assert_equivalent(abstract_code, """
           'Elixir.Darwin.Mutators.Default.OpStrictOrMutator':darwin_was_here('Elixir.MyModule',
                                                                        0, _a@1, _b@1).
           """)

    # Assert the correct number of mutations is generated.
    assert Context.nr_of_mutations(ctx) == 3
  end

  test "operator: not" do
    {abstract_code, ctx} = mutate_elixir("not a")
    # Assert that we generate the correct erlang code.
    assert Erlang.assert_equivalent(abstract_code, """
           'Elixir.Darwin.Mutators.Default.OpStrictNotMutator':darwin_was_here('Elixir.MyModule',
                                                                         0, _a@1).
           """)

    # Assert the correct number of mutations is generated.
    assert Context.nr_of_mutations(ctx) == 3
  end
end
