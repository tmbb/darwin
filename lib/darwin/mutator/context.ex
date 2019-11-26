defmodule Darwin.Mutator.Context do
  @moduledoc """
  The `Context` is a structure used by the mutators to encode state relative to the mutations.
  """
  defstruct mutations: [],
            mutation_count: 0,
            codons: %{},
            module: nil,
            source_path: nil,
            frozen: false,
            mutators: []

  alias Darwin.Mutator.Mutation
  alias Darwin.Mutator.Codon
  alias Darwin.Utils.SafeSourcePath
  alias Darwin.ErlToEx

  @type t :: %__MODULE__{}

  @doc """
  Adds a single mutation to the context.
  """
  @spec add_mutation(t(), integer(), integer(), list()) :: Context.t()
  def add_mutation(%__MODULE__{frozen: false} = ctx, codon_index, mutation_index, opts) do
    %{mutation_count: mutation_count, module: module, mutations: mutations} = ctx

    codon = get_codon(ctx, module, codon_index)
    line = codon.line

    all_opts =
      opts
      |> Keyword.put_new(:module, module)
      |> Keyword.put_new(:index, mutation_index)
      |> Keyword.put_new(:line, line)
      |> Keyword.put_new(:original_codon_index, codon_index)

    mutation = Mutation.new(all_opts)

    %{ctx | mutation_count: mutation_count + 1, mutations: [mutation | mutations]}
  end

  @doc """
  Adds a list of mutations to the context.
  """
  @spec add_mutations(t(), integer(), list()) :: Context.t()
  def add_mutations(%__MODULE__{frozen: false} = ctx, codon_index, opts_list) do
    opts_list
    |> Enum.with_index(0)
    |> Enum.reduce(ctx, fn {mutation_opts, mutation_index}, ctx ->
      add_mutation(ctx, codon_index, mutation_index, mutation_opts)
    end)
  end

  @doc """
  Adds a new codon to a context.
  Returns a pair `{codon, new_ctx}` containing the new codon and the new context.
  """
  @spec new_codon(t(), list()) :: {Codon.t(), t()}
  def new_codon(%__MODULE__{frozen: false} = ctx, opts) do
    %{codons: codons, module: module} = ctx
    next_index = map_size(codons)

    erlang_value = Keyword.get(opts, :value)
    elixir_value = ErlToEx.erl_to_ex(erlang_value)

    value = %{
      erlang: erlang_value,
      elixir: elixir_value
    }

    all_opts =
      opts
      |> Keyword.put(:value, value)
      |> Keyword.put_new(:module, module)
      |> Keyword.put_new(:index, next_index)

    codon = Codon.new(all_opts)
    new_codons = Map.put(codons, {module, next_index}, codon)
    new_ctx = %{ctx | codons: new_codons}
    {codon, new_ctx}
  end

  @doc """
  Fetches a codon from the context (`ctx`), given the `module` and the codon `index`.
  Returns either `{:ok, codon}` or `:error`.

  The behaviour is based on `Map.fetch/2`.
  """
  def fetch_codon(ctx, module, index) do
    %{codons: codons} = ctx
    Map.fetch(codons, {module, index})
  end

  @doc """
  Gets a codon from the context (`ctx`), given the `module` and the codon `index`.
  Returns either the `codon` or `nil`.

  The behaviour is based on `Map.get/2`.
  """
  def get_codon(ctx, module, index) do
    %{codons: codons} = ctx
    Map.get(codons, {module, index})
  end

  @doc """
  Makes it impossible to add new mutations to the context.
  """
  def freeze(%__MODULE__{frozen: false} = ctx) do
    %{ctx | frozen: true}
  end

  @doc """
  Get the number of mutations in a context
  """
  def nr_of_mutations(%__MODULE__{} = ctx) do
    ctx.mutation_count
  end

  @doc """
  Creates a new context
  """
  def new(opts \\ []) do
    module = Keyword.get(opts, :module, nil)

    source_path =
      if module do
        SafeSourcePath.source_path_for_module(module)
      else
        nil
      end

    all_opts = Keyword.merge([source_path: source_path], opts)

    struct(__MODULE__, all_opts)
  end

  @doc """
  Merges several contexts.

  Each module is mutated in its own context.
  This function is useful to merge contexts from different modules.
  """
  def merge(contexts) do
    mutations = Enum.flat_map(contexts, fn ctx -> ctx.mutations end)
    codons = Enum.flat_map(contexts, fn ctx -> ctx.codons end)

    mutations_count =
      contexts
      |> Enum.map(fn %{mutations_count: count} -> count end)
      |> Enum.sum()

    new(
      mutations: mutations,
      codons: codons,
      mutations_count: mutations_count,
      frozen: true
    )
  end

  @doc """
  Fetches the codon for a given `mutation`
  """
  def original_codon(ctx, mutation) do
    %{codons: codons} = ctx
    key = {mutation.module, mutation.original_codon_index}
    Map.fetch!(codons, key)
  end

  @doc """
  Merges the mutations from several contexts.

  Each module is mutated in its own context.
  This function is useful to merge all mutations from different modules.
  """
  def merge_mutations(contexts) do
    Enum.flat_map(contexts, fn ctx -> ctx.mutations end)
  end
end
