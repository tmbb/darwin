defmodule Darwin.DefaultMutators.AbstractCodeTests.ArithmeticOperatorsTest do
  use ExUnit.Case, async: true
  import Darwin.TestHelpers

  alias Darwin.Erlang
  alias Darwin.Mutator.Context

  doctest Darwin

  describe "arithmetic operators (+, -, *, /) - " do
    test "operator: +" do
      {abstract_code, ctx} = mutate_elixir("a + b")
      # Assert that we generate the correct erlang code.
      # We could compare the AST instead, but visual inspection of the code is more informative.
      assert Erlang.assert_equivalent(abstract_code, """
             'Elixir.Darwin.Mutators.Default.OpAddMutator':darwin_was_here('Elixir.MyModule',
                                                                     0, _a@1, _b@1).
             """)

      # Assert the correct number of mutations is generated.
      # One could compare the set of mutations, but that seems a bit useless...
      # We'd be repeating an enormous amount of code for little benefit.
      # Maybe one day (when things are more stable we should add it)
      assert Context.nr_of_mutations(ctx) == 3
    end

    test "operator: -" do
      {abstract_code, ctx} = mutate_elixir("a - b")
      # Assert that we generate the correct erlang code.
      assert Erlang.assert_equivalent(abstract_code, """
             'Elixir.Darwin.Mutators.Default.OpSubMutator':darwin_was_here('Elixir.MyModule',
                                                                     0, _a@1, _b@1).
             """)

      # Assert the correct number of mutations is generated.
      assert Context.nr_of_mutations(ctx) == 4
    end

    test "operator: *" do
      {abstract_code, ctx} = mutate_elixir("a * b")
      # Assert that we generate the correct erlang code.
      assert Erlang.assert_equivalent(abstract_code, """
             'Elixir.Darwin.Mutators.Default.OpMulMutator':darwin_was_here('Elixir.MyModule',
                                                                     0, _a@1, _b@1).
             """)

      # Assert the correct number of mutations is generated.
      assert Context.nr_of_mutations(ctx) == 3
    end

    test "operator: /" do
      {abstract_code, ctx} = mutate_elixir("a / b")
      # Assert that we generate the correct erlang code.
      assert Erlang.assert_equivalent(abstract_code, """
             'Elixir.Darwin.Mutators.Default.OpDivMutator':darwin_was_here('Elixir.MyModule',
                                                                     0, _a@1, _b@1).
             """)

      # Assert the correct number of mutations is generated.
      assert Context.nr_of_mutations(ctx) == 4
    end
  end
end
