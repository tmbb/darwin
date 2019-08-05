defmodule Darwin.Mutators.Default.OpStrictNotMutator do
  alias Darwin.Mutator.Context
  alias Darwin.ActiveMutation
  require Darwin.Mutator, as: Mutator

  def mutate(abstract_code = {:op, line, :not, arg}, ctx) do
    %{module: module} = ctx
    {mutated_arg, ctx} = Mutator.do_mutate(arg, ctx)
    {codon, ctx} = Context.new_codon(ctx, value: abstract_code)
    %{index: codon_index} = codon

    mutated_abstract_code =
      Mutator.call_mutator(
        {__MODULE__, :do_mutate},
        {module, codon_index},
        [mutated_arg],
        line
      )

    mutation_remove_negation = [
      mutator: __MODULE__,
      name: "remove negation",
      mutated_abstract_code: %{
        elixir: nil
      }
    ]

    mutation_replace_by_true = [
      mutator: __MODULE__,
      name: "replace by true",
      mutated_abstract_code: %{
        elixir: quote(do: true),
        erlang: {:atom, line, true}
      }
    ]

    mutation_replace_by_false = [
      mutator: __MODULE__,
      name: "replace by false",
      mutated_abstract_code: %{
        elixir: quote(do: false),
        erlang: {:atom, line, false}
      }
    ]

    mutations = [
      mutation_remove_negation,
      mutation_replace_by_true,
      mutation_replace_by_false
    ]

    ctx = Context.add_mutations(ctx, codon_index, mutations)

    {:ok, {mutated_abstract_code, ctx}}
  end

  def mutate(_abstract_code, _ctx), do: :error

  require Logger

  @doc false
  def do_mutate(module, codon_index, arg) do
    case ActiveMutation.mutation_index_for_codon(module, codon_index) do
      {:ok, 0} -> arg
      {:ok, 1} -> true
      {:ok, 2} -> false
      _ -> not arg
    end
  end
end
