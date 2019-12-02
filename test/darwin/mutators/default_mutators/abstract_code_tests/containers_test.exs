defmodule Darwin.DefaultMutators.AbstractCodeTests.ContainersTest do
  use ExUnit.Case, async: true
  import DarwinTest.Helpers

  alias Darwin.Erlang
  alias Darwin.Mutator.Context

  test "tuple" do
    {abstract_code, ctx} = mutate_elixir("{a, not b, c}")
    # Assert that we generate the correct erlang code.
    assert Erlang.assert_equivalent(abstract_code, """
           {_a@1,
            'Elixir.Darwin.Mutators.Default.OpStrictNotMutator':darwin_was_here('Elixir.MyModule',
                                                                          0, _b@1),
            _c@1}.
           """)

    # Assert the correct number of mutations is generated.
    assert Context.nr_of_mutations(ctx) == 3
  end

  test "list" do
    {abstract_code, ctx} = mutate_elixir("[a, b, not c]")
    # Assert that we generate the correct erlang code.
    assert Erlang.assert_equivalent(abstract_code, """
           [_a@1, _b@1,
            'Elixir.Darwin.Mutators.Default.OpStrictNotMutator':darwin_was_here('Elixir.MyModule',
                                                                          0, _c@1)].
           """)

    # Assert the correct number of mutations is generated.
    assert Context.nr_of_mutations(ctx) == 3
  end
end
