defmodule Darwin.Mutator.Codon do
  defstruct [
    :module,
    :index,
    :value
  ]

  @type t :: %__MODULE__{}

  def new(opts) do
    struct(__MODULE__, opts)
  end
end
