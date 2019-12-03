defmodule Darwin.Mutators.Default.BooleanMutator do
  alias Darwin.Mutator.Context
  alias Darwin.ActiveMutation
  alias Darwin.Mutator

  @behaviour Darwin.Mutator

  @moduledoc """
  Mutates a static boolean.

  It supports the following mutations:

  * Flip boolean: replaces `true` by `false` and `false` by `true`
  * Replaces the boolean value by the atom `:not_a_boolean`
  """

  @impl true
  def mutate({:atom, line, boolean} = abstract_code, %Context{} = ctx)
      when boolean == true or boolean == false do
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

    mutation_flip_boolean = [
      mutator: __MODULE__,
      name: "flip boolean",
      mutated_codon: %{
        elixir: quote(do: unquote(not boolean)),
        erlang: {:atom, line, not boolean}
      }
    ]

    mutation_replace_by_another_atom = [
      mutator: __MODULE__,
      name: "replace by another atom",
      mutated_codon: %{
        elixir: quote(do: :not_a_boolean),
        erlang: {:atom, line, :not_a_boolean}
      }
    ]

    mutations = [
      mutation_flip_boolean,
      mutation_replace_by_another_atom
    ]

    ctx = Context.add_mutations(ctx, codon_index, mutations)

    {:ok, {mutated_abstract_code, ctx}}
  end

  def mutate(_abstract_code, _ctx), do: :error

  @doc false
  def darwin_was_here(module, codon_index, arg) do
    case ActiveMutation.mutation_index_for_codon(module, codon_index) do
      {:ok, 0} -> not arg
      {:ok, 1} -> :not_a_boolean
      _ -> arg
    end
  end
end
