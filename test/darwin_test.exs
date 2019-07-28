defmodule DarwinTest do
  use ExUnit.Case
  alias Darwin.Erlang
  alias Darwin.Mutator.Context
  doctest Darwin

  def mutate(bin, opts \\ [module: MyModule]) do
    module = Keyword.fetch!(opts, :module)

    bin
    |> Erlang.expression!()
    |> Darwin.Mutators.mutate(module)
  end

  # Arithmetic operator replacement

  test "operator: +" do
    {abstract_code, ctx} = mutate("A + B.")
    # Assert that we generate the correct erlang code.
    # We could compare the AST instead, but visual inspection of the code is more informative.
    assert Erlang.pprint(abstract_code) == """
           'Elixir.Darwin.Mutators.Default.OpAddMutator':'__do_mutate__'('Elixir.MyModule',
                                                                         0, A, B)
           """

    # Assert the correct number of mutations is generated.
    # One could compare the set of mutations, but that seems a bit useless...
    # We'd be repeating an enormous amount of code for little benefit.
    # Maybe one day (when things are more stable we should add it)
    assert Context.nr_of_mutations(ctx) == 5
  end

  test "operator: -" do
    {abstract_code, ctx} = mutate("A - B.")
    # Assert that we generate the correct erlang code.
    assert Erlang.pprint(abstract_code) == """
           'Elixir.Darwin.Mutators.Default.OpSubMutator':'__do_mutate__'('Elixir.MyModule',
                                                                         0, A, B)
           """

    # Assert the correct number of mutations is generated.
    assert Context.nr_of_mutations(ctx) == 6
  end

  test "operator: *" do
    {abstract_code, ctx} = mutate("A * B.")
    # Assert that we generate the correct erlang code.
    assert Erlang.pprint(abstract_code) == """
           'Elixir.Darwin.Mutators.Default.OpMulMutator':'__do_mutate__'('Elixir.MyModule',
                                                                         0, A, B)
           """

    # Assert the correct number of mutations is generated.
    assert Context.nr_of_mutations(ctx) == 5
  end

  test "operator: /" do
    {abstract_code, ctx} = mutate("A / B.")
    # Assert that we generate the correct erlang code.
    assert Erlang.pprint(abstract_code) == """
           'Elixir.Darwin.Mutators.Default.OpDivMutator':'__do_mutate__'('Elixir.MyModule',
                                                                         0, A, B)
           """

    # Assert the correct number of mutations is generated.
    assert Context.nr_of_mutations(ctx) == 6
  end

  # Relational operator replacement
end
