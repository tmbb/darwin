defmodule Darwin.Mutator.Context do
  defstruct next_mutation_id: 0,
            mutations: []

  def new(opts \\ []) do
    struct(__MODULE__, opts)
  end
end
