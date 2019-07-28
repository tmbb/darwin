defmodule Darwin.Mutator.Mutation do
  defstruct [
    :mutator,
    :module,
    :index,
    :name,
    :original_codon_index,
    :mutated_codon
  ]

  @type t :: %__MODULE__{}

  def new(opts) do
    struct(__MODULE__, opts)
  end
end
