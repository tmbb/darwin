defmodule Darwin.Mutator.Context do
  defstruct count: 0,
            module: nil,
            mutations: []

  def add_mutation(%__MODULE__{mutations: mutations, count: count} = ctx, data) do
    new_ctx = %{ctx | count: count + 1, mutations: [data | mutations]}
    {count + 1, new_ctx}
  end

  def new(opts \\ []) do
    struct(__MODULE__, opts)
  end
end
