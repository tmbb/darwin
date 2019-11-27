defmodule Darwin.Mutators.Default.OpStrictAndMutator do
  @moduledoc """
  Mutates the `and` operator.
  """
  alias Darwin.Mutator.Context
  alias Darwin.ActiveMutation
  require Darwin.Mutator, as: Mutator

  def mutate(
        abstract_code =
          {:case, line, left,
           [
             _clause1 = {:clause, _, [{:atom, _, false}], [], [{:atom, _, false}]},
             _clause2 = {:clause, _, [{:atom, _, true}], [], [right]},
             _clause3 =
               {:clause, _, [{:var, _, atom1}], [],
                [
                  {:call, _, {:remote, _, {:atom, _, :erlang}, {:atom, _, :error}},
                   [{:tuple, _, [{:atom, _, :badbool}, {:atom, _, :and}, {:var, _, atom2}]}]}
                ]}
           ]},
        ctx
      )
      when atom1 == atom2 do
    {codon, ctx} = Context.new_codon(ctx, value: abstract_code, line: line)
    %{module: module} = ctx
    %{index: codon_index} = codon

    {mutated_left, ctx} = Mutator.do_mutate(left, ctx)
    {mutated_right, ctx} = Mutator.do_mutate(right, ctx)

    mutated_abstract_code =
      Mutator.call_mutator(
        {__MODULE__, :darwin_was_here},
        {module, codon_index},
        [mutated_left, mutated_right],
        line
      )

    mutation_replace_by_or = [
      mutator: __MODULE__,
      name: "replace by or",
      mutated_codon: %{
        elixir: nil
      }
    ]

    mutation_replace_by_true = [
      mutator: __MODULE__,
      name: "replace by true",
      mutated_codon: %{
        elixir: quote(do: true),
        erlang: {:atom, line, true}
      }
    ]

    mutation_replace_by_false = [
      mutator: __MODULE__,
      name: "replace by false",
      mutated_codon: %{
        elixir: quote(do: false),
        erlang: {:atom, line, false}
      }
    ]

    mutations = [
      mutation_replace_by_or,
      mutation_replace_by_true,
      mutation_replace_by_false
    ]

    ctx = Context.add_mutations(ctx, codon_index, mutations)

    {:ok, {mutated_abstract_code, ctx}}
  end

  def mutate(_abstract_code, _ctx), do: :error

  @doc false
  def darwin_was_here(module, codon_index, left, right) do
    case ActiveMutation.mutation_index_for_codon(module, codon_index) do
      {:ok, 0} -> left or right
      {:ok, 1} -> true
      {:ok, 2} -> false
      _ -> left and right
    end
  end
end
