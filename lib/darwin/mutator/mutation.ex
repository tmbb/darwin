defmodule Darwin.Mutator.Mutation do
  @moduledoc """
  TODO: document this
  """
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

  @doc """
  Creates a new mutation.
  """
  @spec new(list({atom(), any()})) :: t()
  def new(opts) do
    struct(__MODULE__, opts)
  end

  @doc """
  Converts a mutation into a tuple.
  The tuple contains the *module*, the *codon index* and the *mutation index*.
  """
  @spec to_tuple(t()) :: {atom(), integer(), integer()}
  def to_tuple(mutation) do
    {mutation.module, mutation.original_codon_index, mutation.index}
  end
end
