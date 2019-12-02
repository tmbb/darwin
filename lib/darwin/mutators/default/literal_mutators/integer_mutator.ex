defmodule Darwin.Mutators.Default.IntegerMutator do
  @moduledoc """
  Mutates a static integer.
  """
  alias Darwin.Mutator.Context
  alias Darwin.ActiveMutation
  alias Darwin.Mutator

  def mutate({:integer, line, integer} = abstract_code, %Context{} = ctx) do
    %{module: module} = ctx
    {codon, ctx} = Context.new_codon(ctx, value: abstract_code, line: line)
    %{index: codon_index} = codon

    mutated_abstract_code =
      Mutator.call_mutator(
        {__MODULE__, :darwin_was_here},
        {module, codon_index},
        [abstract_code],
        line
      )

    mutation_replace_integer = [
      mutator: __MODULE__,
      name: "replace integer",
      mutated_codon: %{
        elixir: runtime_replace_integer(integer),
        erlang: abstract_code_replace_integer(abstract_code)
      }
    ]

    mutations = [
      mutation_replace_integer
    ]

    ctx = Context.add_mutations(ctx, codon_index, mutations)

    {:ok, {mutated_abstract_code, ctx}}
  end

  def mutate(_abstract_code, _ctx), do: :error

  @doc false
  def darwin_was_here(module, codon_index, arg) do
    case ActiveMutation.mutation_index_for_codon(module, codon_index) do
      {:ok, 0} -> runtime_replace_integer(arg)
      _ -> arg
    end
  end

  defp runtime_replace_integer(0), do: 1
  defp runtime_replace_integer(_n), do: 0

  defp abstract_code_replace_integer({:integer, line, integer}) do
    {:integer, line, runtime_replace_integer(integer)}
  end
end
