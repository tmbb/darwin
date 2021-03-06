defmodule Darwin.Mutators.Default.AtomMutator do
  @behaviour Darwin.Mutator
  @moduledoc """
  Mutates a static atom.
  """
  alias Darwin.Mutator.Context
  alias Darwin.ActiveMutation
  alias Darwin.Mutator

  @impl true
  def mutate({:atom, line, atom} = abstract_code, %Context{} = ctx) do
    %{module: module} = ctx
    {codon, ctx} = Context.new_codon(ctx, value: abstract_code, line: line)
    %{index: codon_index} = codon

    mutated_abstract_code =
      Mutator.mutation_for_codon(
        {__MODULE__, :darwin_was_here},
        {module, codon_index},
        [abstract_code],
        line
      )

    mutation_replace_atom = [
      mutator: __MODULE__,
      name: "replace atom",
      mutated_codon: %{
        elixir: runtime_replace_atom(atom),
        erlang: abstract_code_replace_atom(abstract_code)
      }
    ]

    mutations = [
      mutation_replace_atom
    ]

    ctx = Context.add_mutations(ctx, codon_index, mutations)

    {:ok, {mutated_abstract_code, ctx}}
  end

  def mutate(_abstract_code, _ctx), do: :error

  @doc false
  def darwin_was_here(module, codon_index, arg) do
    case ActiveMutation.mutation_index_for_codon(module, codon_index) do
      {:ok, 0} -> runtime_replace_atom(arg)
      _ -> arg
    end
  end

  defp runtime_replace_atom(atom) when atom == :darwin_was_here, do: :darwin_wasnt_here
  defp runtime_replace_atom(_atom), do: :darwin_was_here

  defp abstract_code_replace_atom({:atom, line, atom}) do
    {:atom, line, runtime_replace_atom(atom)}
  end
end
