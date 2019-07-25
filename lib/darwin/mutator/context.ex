defmodule Darwin.Mutator.Context do
  defstruct count: 0,
            module: nil,
            frozen: false,
            mutations: []

  @doc """
  Adds a single mutation to the context.
  """
  def add_mutation(%__MODULE__{frozen: false} = ctx, mutation) do
    %{mutations: mutations, count: count} = ctx
    %{ctx | count: count + 1, mutations: [mutation | mutations]}
  end

  @doc """
  Adds a single mutation to the context.
  """
  def add_mutations(%__MODULE__{frozen: false} = ctx, mutations) do
    Enum.reduce(mutations, ctx, fn mutation, ctx ->
      add_mutation(ctx, mutation)
    end)
  end

  @doc """
  Reverse the mutation list, so that mutations appear in the corrrect order
  and makes it impossible to add new mutations to the context.
  """
  def freeze(%__MODULE__{frozen: false} = ctx) do
    %{ctx | mutations: :lists.reverse(ctx.mutations), frozen: true}
  end

  @doc """
  Get the number of mutations in a context
  """
  def nr_of_mutations(%__MODULE__{} = ctx) do
    ctx.count
  end

  def new(opts \\ []) do
    struct(__MODULE__, opts)
  end
end
