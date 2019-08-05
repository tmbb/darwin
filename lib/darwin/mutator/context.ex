defmodule Darwin.Mutator.Context do
  defstruct mutations: [],
            mutation_count: 0,
            codons: %{},
            module: nil,
            frozen: false,
            mutators: []

  alias Darwin.Mutator.Mutation
  alias Darwin.Mutator.Codon

  @type t :: %__MODULE__{}

  @doc """
  Adds a single mutation to the context.
  """
  @spec add_mutation(t(), integer(), integer(), list()) :: Context.t()
  def add_mutation(%__MODULE__{frozen: false} = ctx, codon_index, mutation_index, opts) do
    %{mutation_count: mutation_count, module: module, mutations: mutations} = ctx

    all_opts =
      opts
      |> Keyword.put_new(:module, module)
      |> Keyword.put_new(:index, mutation_index)
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

  @spec new_codon(t(), list()) :: {Codon.t(), t()}
  def new_codon(%__MODULE__{frozen: false} = ctx, opts) do
    %{codons: codons, module: module} = ctx
    next_index = map_size(codons)

    all_opts =
      opts
      |> Keyword.put_new(:module, module)
      |> Keyword.put_new(:index, next_index)

    codon = Codon.new(all_opts)
    new_codons = Map.put(codons, {module, next_index}, codon)
    new_ctx = %{ctx | codons: new_codons}
    {codon, new_ctx}
  end

  def fetch_codon(ctx, module, index) do
    %{codons: codons} = ctx
    Map.fetch(codons, {module, index})
  end

  def get_codon(ctx, module, index) do
    %{codons: codons} = ctx
    Map.get(codons, {module, index})
  end

  @doc """
  Reverse the mutation list, so that mutations appear in the corrrect order
  and makes it impossible to add new mutations to the context.
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

  def new(opts \\ []) do
    struct(__MODULE__, opts)
  end
end
