defmodule Darwin.DefaultMutators.AbstractCodeTests.ComparisonOperatorsTest do
  use ExUnit.Case, async: true
  # import DarwinTest.Helpers

  # alias Darwin.Erlang
  # alias Darwin.Mutator.Context

  describe "comparison operators (<, <=, ==, !=, >, >=) -" do
    # test "operator: <" do
    #   {abstract_code, ctx} = mutate_erlang("A < B.")
    #   # Assert that we generate the correct erlang code.
    #   # We could compare the AST instead, but visual inspection of the code is more informative.
    #   assert Erlang.pprint(abstract_code) == """
    #          'Elixir.Darwin.Mutators.Default.OpLessThanMutator':do_mutate('Elixir.MyModule',
    #                                                                             0, 0, A, B)
    #          """

    #   # Assert the correct number of mutations is generated.
    #   # One could compare the set of mutations, but that seems a bit useless...
    #   # We'd be repeating an enormous amount of code for little benefit.
    #   # Maybe one day (when things are more stable we should add it)
    #   assert Context.nr_of_mutations(ctx) == 8
    # end

    # test "operator: <=" do
    #   {abstract_code, ctx} = mutate("A =< B.")
    #   # Assert that we generate the correct erlang code.
    #   assert Erlang.pprint(abstract_code) == """
    #          'Elixir.Darwin.Mutators.Default.OpLessThanOrEqualToMutator':do_mutate('Elixir.MyModule',
    #                                                                                      0,
    #                                                                                      0,
    #                                                                                      A,
    #                                                                                      B)
    #          """

    #   # Assert the correct number of mutations is generated.
    #   assert Context.nr_of_mutations(ctx) == 8
    # end

    # test "operator: ==" do
    #   {abstract_code, ctx} = mutate("A == B.")
    #   # Assert that we generate the correct erlang code.
    #   assert Erlang.pprint(abstract_code) == """
    #          'Elixir.Darwin.Mutators.Default.OpEqualToMutator':do_mutate('Elixir.MyModule',
    #                                                                            0, 0, A, B)
    #          """

    #   # Assert the correct number of mutations is generated.
    #   assert Context.nr_of_mutations(ctx) == 7
    # end

    # test "operator: !=" do
    #   {abstract_code, ctx} = mutate("A /= B.")
    #   # Assert that we generate the correct erlang code.
    #   assert Erlang.pprint(abstract_code) == """
    #          'Elixir.Darwin.Mutators.Default.OpNotEqualToMutator':do_mutate('Elixir.MyModule',
    #                                                                               0, 0, A, B)
    #          """

    #   # Assert the correct number of mutations is generated.
    #   assert Context.nr_of_mutations(ctx) == 8
    # end

    # test "operator: >=" do
    #   {abstract_code, ctx} = mutate("A >= B.")
    #   # Assert that we generate the correct erlang code.
    #   assert Erlang.pprint(abstract_code) == """
    #          'Elixir.Darwin.Mutators.Default.OpGreaterThanOrEqualToMutator':do_mutate('Elixir.MyModule',
    #                                                                                         0,
    #                                                                                         0,
    #                                                                                         A,
    #                                                                                         B)
    #          """

    #   # Assert the correct number of mutations is generated.
    #   assert Context.nr_of_mutations(ctx) == 8
    # end

    # test "operator: >" do
    #   {abstract_code, ctx} = mutate("A > B.")
    #   # Assert that we generate the correct erlang code.
    #   assert Erlang.pprint(abstract_code) == """
    #          'Elixir.Darwin.Mutators.Default.OpGreaterThanMutator':do_mutate('Elixir.MyModule',
    #                                                                                0, 0, A,
    #                                                                                B)
    #          """

    #   # Assert the correct number of mutations is generated.
    #   assert Context.nr_of_mutations(ctx) == 8
    # end
  end
end
