defmodule Darwin.DefaultMutators.AbstractCodeTests.LiteralsTest do
  use ExUnit.Case, async: true
  import DarwinTest.Helpers

  alias Darwin.Erlang
  alias Darwin.Mutator.Context

  describe "literals - " do
    test "charlist (erlang)" do
      {abstract_code, ctx} = mutate_erlang(~s'"galapagos".')
      # Assert that we generate the correct erlang code.
      assert Erlang.pprint(abstract_code) == """
             'Elixir.Darwin.Mutators.Default.CharlistMutator':do_mutate('Elixir.MyModule',
                                                                        0, "galapagos")
             """

      # Assert the correct number of mutations is generated.
      assert Context.nr_of_mutations(ctx) == 1
    end

    test "charlist (elixir)" do
      # Elixir doesn't compile a charlist to an Erlang string!
      {abstract_code, ctx} = mutate_elixir("'galapagos'")
      # Assert that we generate the correct erlang code.
      assert Erlang.pprint(abstract_code) == """
             [103, 97, 108, 97, 112, 97, 103, 111, 115]
             """

      # Assert the correct number of mutations is generated.
      assert Context.nr_of_mutations(ctx) == 0
    end

    test "binary (erlang)" do
      {abstract_code, ctx} = mutate_erlang(~s'<<"galapagos">>.')
      # Assert that we generate the correct erlang code.
      assert Erlang.pprint(abstract_code) == """
             'Elixir.Darwin.Mutators.Default.StringMutator':do_mutate('Elixir.MyModule',
                                                                      0, <<"galapagos">>)
             """

      # Assert the correct number of mutations is generated.
      assert Context.nr_of_mutations(ctx) == 1
    end

    test "atom (erlang)" do
      {abstract_code, ctx} = mutate_erlang("evolution.")
      # Assert that we generate the correct erlang code.
      assert Erlang.pprint(abstract_code) == """
             'Elixir.Darwin.Mutators.Default.AtomMutator':do_mutate('Elixir.MyModule',
                                                                    0, evolution)
             """

      # Assert the correct number of mutations is generated.
      assert Context.nr_of_mutations(ctx) == 1
    end

    test "atom (elixir)" do
      {abstract_code, ctx} = mutate_elixir(":evolution")
      # Assert that we generate the correct erlang code.
      assert Erlang.pprint(abstract_code) == """
             'Elixir.Darwin.Mutators.Default.AtomMutator':do_mutate('Elixir.MyModule',
                                                                    0, evolution)
             """

      # Assert the correct number of mutations is generated.
      assert Context.nr_of_mutations(ctx) == 1
    end
  end
end
