defmodule DarwinTest.Helpers do
  @doc """
  Helpers to make it easier to mutate Elixir and Erlang expressions
  """

  alias Darwin.Mutator
  alias Darwin.Erlang

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

  def mutate_elixir_module(module_name, body, _opts \\ []) do
    module_name
    |> ExToErl.elixir_module_source_to_erlang_abstract_code(body)
    |> Mutator.mutate(module_name)
  end
end
