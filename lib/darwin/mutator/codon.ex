defmodule Darwin.Mutator.Codon do
  @moduledoc """
  A *codon* is an AST node that can be mutated.
  """
  defstruct [
    :module,
    :index,
    :value,
    :line
  ]

  @type t :: %__MODULE__{}

  @doc """
  Creates a new codon.
  """
  def new(opts) do
    struct(__MODULE__, opts)
  end
end
