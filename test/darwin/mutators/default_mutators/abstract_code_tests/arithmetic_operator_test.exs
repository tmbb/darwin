defmodule Darwin.DefaultMutators.AbstractCodeTests.ArithmeticOperatorsTest do
  use ExUnit.Case, async: true
  import DarwinTest.Helpers

  alias Darwin.Erlang
  alias Darwin.Mutator.Context

  doctest Darwin

  describe "arithmetic operators (+, -, *, /) - " do
    test "operator: +" do
      {abstract_code, ctx} = mutate_elixir("a + b")
      # Assert that we generate the correct erlang code.
      # We could compare the AST instead, but visual inspection of the code is more informative.
      assert Erlang.equivalent?(abstract_code, """
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
      assert Erlang.equivalent?(abstract_code, """
             'Elixir.Darwin.Mutators.Default.OpSubMutator':darwin_was_here('Elixir.MyModule',
                                                                     0, _a@1, _b@1).
             """)

      # Assert the correct number of mutations is generated.
      assert Context.nr_of_mutations(ctx) == 4
    end

    test "operator: *" do
      {abstract_code, ctx} = mutate_elixir("a * b")
      # Assert that we generate the correct erlang code.
      assert Erlang.equivalent?(abstract_code, """
             'Elixir.Darwin.Mutators.Default.OpMulMutator':darwin_was_here('Elixir.MyModule',
                                                                     0, _a@1, _b@1).
             """)

      # Assert the correct number of mutations is generated.
      assert Context.nr_of_mutations(ctx) == 3
    end

    test "operator: /" do
      {abstract_code, ctx} = mutate_elixir("a / b")
      # Assert that we generate the correct erlang code.
      assert Erlang.equivalent?(abstract_code, """
             'Elixir.Darwin.Mutators.Default.OpDivMutator':darwin_was_here('Elixir.MyModule',
                                                                     0, _a@1, _b@1).
             """)

      # Assert the correct number of mutations is generated.
      assert Context.nr_of_mutations(ctx) == 4
    end
  end

  describe "comparison operators (<, <=, ==, !=, >, >=) -" do
    # test "operator: <" do
    #   {abstract_code, ctx} = mutate_erlang("A < B.")
    #   # Assert that we generate the correct erlang code.
    #   # We could compare the AST instead, but visual inspection of the code is more informative.
    #   assert Erlang.pprint(abstract_code) == """
    #          'Elixir.Darwin.Mutators.Default.OpLessThanMutator':darwin_was_here('Elixir.MyModule',
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
    #          'Elixir.Darwin.Mutators.Default.OpLessThanOrEqualToMutator':darwin_was_here('Elixir.MyModule',
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
    #          'Elixir.Darwin.Mutators.Default.OpEqualToMutator':darwin_was_here('Elixir.MyModule',
    #                                                                            0, 0, A, B)
    #          """

    #   # Assert the correct number of mutations is generated.
    #   assert Context.nr_of_mutations(ctx) == 7
    # end

    # test "operator: !=" do
    #   {abstract_code, ctx} = mutate("A /= B.")
    #   # Assert that we generate the correct erlang code.
    #   assert Erlang.pprint(abstract_code) == """
    #          'Elixir.Darwin.Mutators.Default.OpNotEqualToMutator':darwin_was_here('Elixir.MyModule',
    #                                                                               0, 0, A, B)
    #          """

    #   # Assert the correct number of mutations is generated.
    #   assert Context.nr_of_mutations(ctx) == 8
    # end

    # test "operator: >=" do
    #   {abstract_code, ctx} = mutate("A >= B.")
    #   # Assert that we generate the correct erlang code.
    #   assert Erlang.pprint(abstract_code) == """
    #          'Elixir.Darwin.Mutators.Default.OpGreaterThanOrEqualToMutator':darwin_was_here('Elixir.MyModule',
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
    #          'Elixir.Darwin.Mutators.Default.OpGreaterThanMutator':darwin_was_here('Elixir.MyModule',
    #                                                                                0, 0, A,
    #                                                                                B)
    #          """

    #   # Assert the correct number of mutations is generated.
    #   assert Context.nr_of_mutations(ctx) == 8
    # end
  end

  describe "strict logical operators (and, or, not) -" do
    test "operator: and" do
      {abstract_code, ctx} = mutate_elixir("a and b")
      # Assert that we generate the correct erlang code.
      assert Erlang.equivalent?(abstract_code, """
             'Elixir.Darwin.Mutators.Default.OpStrictAndMutator':darwin_was_here('Elixir.MyModule',
                                                                           0, _a@1, _b@1).
             """)

      # Assert the correct number of mutations is generated.
      assert Context.nr_of_mutations(ctx) == 3
    end

    test "operator: or" do
      {abstract_code, ctx} = mutate_elixir("a or b")
      # Assert that we generate the correct erlang code.
      assert Erlang.equivalent?(abstract_code, """
             'Elixir.Darwin.Mutators.Default.OpStrictOrMutator':darwin_was_here('Elixir.MyModule',
                                                                          0, _a@1, _b@1).
             """)

      # Assert the correct number of mutations is generated.
      assert Context.nr_of_mutations(ctx) == 3
    end

    test "operator: not" do
      {abstract_code, ctx} = mutate_elixir("not a")
      # Assert that we generate the correct erlang code.
      assert Erlang.equivalent?(abstract_code, """
             'Elixir.Darwin.Mutators.Default.OpStrictNotMutator':darwin_was_here('Elixir.MyModule',
                                                                           0, _a@1).
             """)

      # Assert the correct number of mutations is generated.
      assert Context.nr_of_mutations(ctx) == 3
    end
  end

  describe "literals - " do
    test "charlist (erlang)" do
      {abstract_code, ctx} = mutate_erlang(~s'"galapagos".')
      # Assert that we generate the correct erlang code.
      assert Erlang.equivalent?(abstract_code, """
             'Elixir.Darwin.Mutators.Default.CharlistMutator':darwin_was_here('Elixir.MyModule',
                                                                        0, "galapagos").
             """)

      # Assert the correct number of mutations is generated.
      assert Context.nr_of_mutations(ctx) == 1
    end

    test "charlist (elixir)" do
      # Elixir doesn't compile a charlist to an Erlang string!
      {abstract_code, ctx} = mutate_elixir("'galapagos'")
      # Assert that we generate the correct erlang code.
      assert Erlang.equivalent?(abstract_code, """
             [103, 97, 108, 97, 112, 97, 103, 111, 115].
             """)

      # Assert the correct number of mutations is generated.
      assert Context.nr_of_mutations(ctx) == 0
    end

    test "binary (erlang)" do
      {abstract_code, ctx} = mutate_erlang(~s'<<"galapagos">>.')
      # Assert that we generate the correct erlang code.
      assert Erlang.equivalent?(abstract_code, """
             'Elixir.Darwin.Mutators.Default.StringMutator':darwin_was_here('Elixir.MyModule',
                                                                      0, <<"galapagos">>).
             """)

      # Assert the correct number of mutations is generated.
      assert Context.nr_of_mutations(ctx) == 1
    end

    test "atom (erlang)" do
      {abstract_code, ctx} = mutate_erlang("evolution.")
      # Assert that we generate the correct erlang code.
      assert Erlang.equivalent?(abstract_code, """
             'Elixir.Darwin.Mutators.Default.AtomMutator':darwin_was_here('Elixir.MyModule',
                                                                    0, evolution).
             """)

      # Assert the correct number of mutations is generated.
      assert Context.nr_of_mutations(ctx) == 1
    end

    test "atom (elixir)" do
      {abstract_code, ctx} = mutate_elixir(":evolution")
      # Assert that we generate the correct erlang code.
      assert Erlang.equivalent?(abstract_code, """
             'Elixir.Darwin.Mutators.Default.AtomMutator':darwin_was_here('Elixir.MyModule',
                                                                    0, evolution).
             """)

      # Assert the correct number of mutations is generated.
      assert Context.nr_of_mutations(ctx) == 1
    end
  end

  describe "containers -" do
    test "tuple" do
      {abstract_code, ctx} = mutate_elixir("{a, not b, c}")
      # Assert that we generate the correct erlang code.
      assert Erlang.equivalent?(abstract_code, """
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
      assert Erlang.equivalent?(abstract_code, """
             [_a@1, _b@1,
              'Elixir.Darwin.Mutators.Default.OpStrictNotMutator':darwin_was_here('Elixir.MyModule',
                                                                            0, _c@1)].
             """)

      # Assert the correct number of mutations is generated.
      assert Context.nr_of_mutations(ctx) == 3
    end
  end
end
