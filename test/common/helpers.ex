defmodule DarwinTest.Helpers do
  @doc """
  Helpers to make it easier to mutate Elixir and Erlang expressions
  """

  alias Darwin.Mutator
  alias Darwin.Erlang
  alias Darwin.ExToErl
  alias Darwin.Beam

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

  def compile_elixir(arg, opts \\ [module: MyModule])

  def compile_elixir(bin, opts) when is_binary(bin) do
    module = Keyword.fetch!(opts, :module)
    body = Code.string_to_quoted!(bin)
    Module.create(module, body, Macro.Env.location(__ENV__))
  end

  def compile_elixir(body, opts) do
    module = Keyword.fetch!(opts, :module)
    Module.create(module, body, Macro.Env.location(__ENV__))
  end

  def mutate_and_compile_elixir(arg, opts \\ [module: MyModule])

  def mutate_and_compile_elixir(bin, opts) when is_binary(bin) do
    body = Code.string_to_quoted!(bin)
    mutate_and_compile_elixir(body, opts)
  end

  def mutate_and_compile_elixir(body, opts) do
    module = Keyword.fetch!(opts, :module)
    {:module, _, chunks, _} = compile_elixir(body, opts)

    {:ok, {_, [{:abstract_code, {_, abstract_code}}]}} =
      :beam_lib.chunks(chunks, [:abstract_code])

    {mutated_abstract_code, ctx} = Mutator.mutate(abstract_code, module)

    Beam.compile_and_load(mutated_abstract_code)

    {mutated_abstract_code, ctx}
  end

  def mutate_elixir_module(module_name, body, _opts \\ []) do
    module_name
    |> ExToErl.elixir_module_source_to_erlang_abstract_code(body)
    |> Mutator.mutate(module_name)
  end
end
