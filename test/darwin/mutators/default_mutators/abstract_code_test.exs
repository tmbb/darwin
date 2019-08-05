defmodule Darwin.DefaultMutators.AbstractCodeTest do
  use ExUnit.Case
  alias Darwin.Erlang
  alias Darwin.Mutator
  alias Darwin.Mutator.Context
  doctest Darwin

  def mutate_erlang(bin, opts \\ [module: MyModule]) do
    module = Keyword.fetch!(opts, :module)

    bin
    |> Erlang.expression!()
    |> Mutator.mutate(module)
  end

  def mutate_elixir(bin, opts \\ [module: MyModule]) do
    module = Keyword.fetch!(opts, :module)

    bin
    |> ExToErl.elixir_source_to_erlang_abstract_code()
    |> Mutator.mutate(module)
  end

  # describe "arithmetic operators (+, -, *, /) - " do
  #   test "operator: +" do
  #     {abstract_code, ctx} = mutate("A + B.")
  #     # Assert that we generate the correct erlang code.
  #     # We could compare the AST instead, but visual inspection of the code is more informative.
  #     assert Erlang.pprint(abstract_code) == """
  #            'Elixir.Darwin.Mutators.Default.OpAddMutator':do_mutate('Elixir.MyModule',
  #                                                                          0, 0, A, B)
  #            """

  #     # Assert the correct number of mutations is generated.
  #     # One could compare the set of mutations, but that seems a bit useless...
  #     # We'd be repeating an enormous amount of code for little benefit.
  #     # Maybe one day (when things are more stable we should add it)
  #     assert Context.nr_of_mutations(ctx) == 5
  #   end

  #   test "operator: -" do
  #     {abstract_code, ctx} = mutate("A - B.")
  #     # Assert that we generate the correct erlang code.
  #     assert Erlang.pprint(abstract_code) == """
  #            'Elixir.Darwin.Mutators.Default.OpSubMutator':do_mutate('Elixir.MyModule',
  #                                                                          0, 0, A, B)
  #            """

  #     # Assert the correct number of mutations is generated.
  #     assert Context.nr_of_mutations(ctx) == 6
  #   end

  #   test "operator: *" do
  #     {abstract_code, ctx} = mutate("A * B.")
  #     # Assert that we generate the correct erlang code.
  #     assert Erlang.pprint(abstract_code) == """
  #            'Elixir.Darwin.Mutators.Default.OpMulMutator':do_mutate('Elixir.MyModule',
  #                                                                          0, 0, A, B)
  #            """

  #     # Assert the correct number of mutations is generated.
  #     assert Context.nr_of_mutations(ctx) == 5
  #   end

  #   test "operator: /" do
  #     {abstract_code, ctx} = mutate("A / B.")
  #     # Assert that we generate the correct erlang code.
  #     assert Erlang.pprint(abstract_code) == """
  #            'Elixir.Darwin.Mutators.Default.OpDivMutator':do_mutate('Elixir.MyModule',
  #                                                                          0, 0, A, B)
  #            """

  #     # Assert the correct number of mutations is generated.
  #     assert Context.nr_of_mutations(ctx) == 6
  #   end
  # end

  # describe "comparison operators (<, <=, ==, !=, >, >=) -" do
  #   test "operator: <" do
  #     {abstract_code, ctx} = mutate("A < B.")
  #     # Assert that we generate the correct erlang code.
  #     # We could compare the AST instead, but visual inspection of the code is more informative.
  #     assert Erlang.pprint(abstract_code) == """
  #            'Elixir.Darwin.Mutators.Default.OpLessThanMutator':do_mutate('Elixir.MyModule',
  #                                                                               0, 0, A, B)
  #            """

  #     # Assert the correct number of mutations is generated.
  #     # One could compare the set of mutations, but that seems a bit useless...
  #     # We'd be repeating an enormous amount of code for little benefit.
  #     # Maybe one day (when things are more stable we should add it)
  #     assert Context.nr_of_mutations(ctx) == 8
  #   end

  #   test "operator: <=" do
  #     {abstract_code, ctx} = mutate("A =< B.")
  #     # Assert that we generate the correct erlang code.
  #     assert Erlang.pprint(abstract_code) == """
  #            'Elixir.Darwin.Mutators.Default.OpLessThanOrEqualToMutator':do_mutate('Elixir.MyModule',
  #                                                                                        0,
  #                                                                                        0,
  #                                                                                        A,
  #                                                                                        B)
  #            """

  #     # Assert the correct number of mutations is generated.
  #     assert Context.nr_of_mutations(ctx) == 8
  #   end

  #   test "operator: ==" do
  #     {abstract_code, ctx} = mutate("A == B.")
  #     # Assert that we generate the correct erlang code.
  #     assert Erlang.pprint(abstract_code) == """
  #            'Elixir.Darwin.Mutators.Default.OpEqualToMutator':do_mutate('Elixir.MyModule',
  #                                                                              0, 0, A, B)
  #            """

  #     # Assert the correct number of mutations is generated.
  #     assert Context.nr_of_mutations(ctx) == 7
  #   end

  #   test "operator: !=" do
  #     {abstract_code, ctx} = mutate("A /= B.")
  #     # Assert that we generate the correct erlang code.
  #     assert Erlang.pprint(abstract_code) == """
  #            'Elixir.Darwin.Mutators.Default.OpNotEqualToMutator':do_mutate('Elixir.MyModule',
  #                                                                                 0, 0, A, B)
  #            """

  #     # Assert the correct number of mutations is generated.
  #     assert Context.nr_of_mutations(ctx) == 8
  #   end

  #   test "operator: >=" do
  #     {abstract_code, ctx} = mutate("A >= B.")
  #     # Assert that we generate the correct erlang code.
  #     assert Erlang.pprint(abstract_code) == """
  #            'Elixir.Darwin.Mutators.Default.OpGreaterThanOrEqualToMutator':do_mutate('Elixir.MyModule',
  #                                                                                           0,
  #                                                                                           0,
  #                                                                                           A,
  #                                                                                           B)
  #            """

  #     # Assert the correct number of mutations is generated.
  #     assert Context.nr_of_mutations(ctx) == 8
  #   end

  #   test "operator: >" do
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
  # end

  describe "strict logical operators (and, or, not) -" do
    test "operator: and" do
      {abstract_code, ctx} = mutate_elixir("a and b")
      # Assert that we generate the correct erlang code.
      assert Erlang.pprint(abstract_code) == """
             'Elixir.Darwin.Mutators.Default.OpStrictAndMutator':do_mutate('Elixir.MyModule',
                                                                           0, _a@1, _b@1)
             """

      # Assert the correct number of mutations is generated.
      assert Context.nr_of_mutations(ctx) == 3
    end

    test "operator: or" do
      {abstract_code, ctx} = mutate_elixir("a or b")
      # Assert that we generate the correct erlang code.
      assert Erlang.pprint(abstract_code) == """
             'Elixir.Darwin.Mutators.Default.OpStrictOrMutator':do_mutate('Elixir.MyModule',
                                                                          0, _a@1, _b@1)
             """

      # Assert the correct number of mutations is generated.
      assert Context.nr_of_mutations(ctx) == 3
    end

    test "operator: not" do
      {abstract_code, ctx} = mutate_elixir("not a")
      # Assert that we generate the correct erlang code.
      assert Erlang.pprint(abstract_code) == """
             'Elixir.Darwin.Mutators.Default.OpStrictNotMutator':do_mutate('Elixir.MyModule',
                                                                           0, _a@1)
             """

      # Assert the correct number of mutations is generated.
      assert Context.nr_of_mutations(ctx) == 3
    end
  end

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

  describe "containers -" do
    test "tuple" do
      {abstract_code, ctx} = mutate_elixir("{a, not b, c}")
      # Assert that we generate the correct erlang code.
      assert Erlang.pprint(abstract_code) == """
             {_a@1,
              'Elixir.Darwin.Mutators.Default.OpStrictNotMutator':do_mutate('Elixir.MyModule',
                                                                            0, _b@1),
              _c@1}
             """

      # Assert the correct number of mutations is generated.
      assert Context.nr_of_mutations(ctx) == 3
    end

    test "list" do
      {abstract_code, ctx} = mutate_elixir("[a, b, not c]")
      # Assert that we generate the correct erlang code.
      assert Erlang.pprint(abstract_code) == """
             [_a@1, _b@1,
              'Elixir.Darwin.Mutators.Default.OpStrictNotMutator':do_mutate('Elixir.MyModule',
                                                                            0, _c@1)]
             """

      # Assert the correct number of mutations is generated.
      assert Context.nr_of_mutations(ctx) == 3
    end
  end
end
