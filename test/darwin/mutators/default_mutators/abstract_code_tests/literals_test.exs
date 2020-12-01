defmodule Darwin.DefaultMutators.AbstractCodeTests.LiteralsTest do
  use ExUnit.Case, async: true
  import Darwin.TestHelpers

  alias Darwin.Erlang
  alias Darwin.Mutator.Context

  test "charlist (erlang)" do
    {abstract_code, ctx} = mutate_erlang(~s'"galapagos".')
    # Assert that we generate the correct erlang code.
    assert Erlang.assert_equivalent(abstract_code, """
           'Elixir.Darwin.Mutators.Default.CharlistMutator':darwin_was_here(
             'Elixir.MyModule',
             0,
             "galapagos"
            ).
           """)

    # Assert the correct number of mutations is generated.
    assert Context.nr_of_mutations(ctx) == 1
  end

  test "charlist (elixir)" do
    # Elixir doesn't compile a charlist to an Erlang string!
    {abstract_code, ctx} = mutate_elixir("'galapagos'")
    # Assert that we generate the correct erlang code.
    assert Erlang.assert_equivalent(
             abstract_code,
             """
             [
               'Elixir.Darwin.Mutators.Default.IntegerMutator':darwin_was_here('Elixir.MyModule', 0, 103),
               'Elixir.Darwin.Mutators.Default.IntegerMutator':darwin_was_here('Elixir.MyModule', 1, 97),
               'Elixir.Darwin.Mutators.Default.IntegerMutator':darwin_was_here('Elixir.MyModule', 2, 108),
               'Elixir.Darwin.Mutators.Default.IntegerMutator':darwin_was_here('Elixir.MyModule', 3, 97),
               'Elixir.Darwin.Mutators.Default.IntegerMutator':darwin_was_here('Elixir.MyModule', 4, 112),
               'Elixir.Darwin.Mutators.Default.IntegerMutator':darwin_was_here('Elixir.MyModule', 5, 97),
               'Elixir.Darwin.Mutators.Default.IntegerMutator':darwin_was_here('Elixir.MyModule', 6, 103),
               'Elixir.Darwin.Mutators.Default.IntegerMutator':darwin_was_here('Elixir.MyModule', 7, 111),
               'Elixir.Darwin.Mutators.Default.IntegerMutator':darwin_was_here('Elixir.MyModule', 8, 115)
              ].
             """
           )

    # Assert the correct number of mutations is generated.
    assert Context.nr_of_mutations(ctx) == 9
  end

  test "binary (erlang)" do
    {abstract_code, ctx} = mutate_erlang(~s'<<"galapagos">>.')
    # Assert that we generate the correct erlang code.
    assert Erlang.assert_equivalent(abstract_code, """
           'Elixir.Darwin.Mutators.Default.StringMutator':darwin_was_here('Elixir.MyModule',
                                                                    0, <<"galapagos">>).
           """)

    # Assert the correct number of mutations is generated.
    assert Context.nr_of_mutations(ctx) == 1
  end

  test "atom (erlang)" do
    {abstract_code, ctx} = mutate_erlang("evolution.")
    # Assert that we generate the correct erlang code.
    assert Erlang.assert_equivalent(abstract_code, """
           'Elixir.Darwin.Mutators.Default.AtomMutator':darwin_was_here('Elixir.MyModule',
                                                                  0, evolution).
           """)

    # Assert the correct number of mutations is generated.
    assert Context.nr_of_mutations(ctx) == 1
  end

  test "atom (elixir)" do
    {abstract_code, ctx} = mutate_elixir(":evolution")
    # Assert that we generate the correct erlang code.
    assert Erlang.assert_equivalent(abstract_code, """
           'Elixir.Darwin.Mutators.Default.AtomMutator':darwin_was_here('Elixir.MyModule',
                                                                  0, evolution).
           """)

    # Assert the correct number of mutations is generated.
    assert Context.nr_of_mutations(ctx) == 1
  end
end
