defmodule Darwin.Mutator.Mutant do
  @moduledoc """
  TODO: document this
  """
  defstruct [
    :mutation,
    :original_codon,
    :module,
    :line,
    :state
  ]

  @type t :: %__MODULE__{}

  @doc """
  Creates a new mutation.

  Takes the following required options:
  * `mutation`
  * `state` (which can be `:killed` or `:`)
  """
  @spec new(list({atom(), any()})) :: t()
  def new(opts) do
    mutation = Keyword.fetch!(opts, :mutation)
    state = Keyword.fetch!(opts, :state)
    original_codon = Keyword.fetch!(opts, :original_codon)
    line = mutation.line
    module = mutation.module

    %__MODULE__{
      mutation: mutation,
      original_codon: original_codon,
      module: module,
      line: line,
      state: state
    }
  end

  @spec group_by_module(list(t())) :: Map.t(atom(), list(t()))
  def group_by_module(mutants) do
    Enum.group_by(mutants, fn mutant ->
      mutant.module
    end)
  end
end
