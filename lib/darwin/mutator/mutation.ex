defmodule Darwin.Mutator.Mutation do
  defstruct [
    :mutator,
    :module,
    :index,
    :line,
    :name,
    :original_codon_index,
    :mutated_codon
  ]

  @type t :: %__MODULE__{}

  @spec new(list({atom(), any()})) :: t()
  def new(opts) do
    struct(__MODULE__, opts)
  end

  @spec to_tuple(t()) :: {atom(), integer(), integer()}
  def to_tuple(mutation) do
    {mutation.module, mutation.original_codon_index, mutation.index}
  end
end
