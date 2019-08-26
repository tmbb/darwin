defmodule Darwin.Mutator.Codon do
  defstruct [
    :module,
    :index,
    :value,
    :line
  ]

  @type t :: %__MODULE__{}

  def new(opts) do
    struct(__MODULE__, opts)
  end
end
