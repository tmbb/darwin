defmodule Darwin.Mutators.Default.OpLaxOrMutator do
  alias Darwin.Mutator.Context
  alias Darwin.ActiveMutation
  alias Darwin.ErlToEx
  require Darwin.Mutator, as: Mutator

  def mutate(
        abstract_code =
          {:case, line, left,
           [
             {:clause, _, [{:var, _, atom1}],
              [
                [
                  {:op, _, :orelse, {:op, _, :"=:=", {:var, _, atom2}, {:atom, _, nil}},
                   {:op, _, :"=:=", {:var, _, atom3}, {:atom, _, false}}}
                ]
              ], [right]},
             {:clause, _, [{:var, _, atom4}], [], [{:var, _, atom5}]}
           ]},
        ctx
      )
      when atom1 == atom2 and atom2 == atom3 and atom4 == atom5 do
    {codon, ctx} = Context.new_codon(ctx, value: abstract_code, line: line)
    %{module: module} = ctx
    %{index: codon_index} = codon

    elixir_left = ErlToEx.erl_to_ex(left)
    elixir_right = ErlToEx.erl_to_ex(right)

    {mutated_left, ctx} = Mutator.do_mutate(left, ctx)
    {mutated_right, ctx} = Mutator.do_mutate(right, ctx)

    mutated_abstract_code =
      Mutator.call_mutator(
        {__MODULE__, :do_mutate},
        {module, codon_index},
        [mutated_left, mutated_right],
        line
      )

    mutation_replace_by_and = [
      mutator: __MODULE__,
      name: "replace by &&",
      mutated_codon: %{
        elixir: quote(do: unquote(elixir_left) && unquote(elixir_right))
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
      mutation_replace_by_and,
      mutation_replace_by_true,
      mutation_replace_by_false
    ]

    ctx = Context.add_mutations(ctx, codon_index, mutations)

    {:ok, {mutated_abstract_code, ctx}}
  end

  def mutate(_abstract_code, _ctx), do: :error

  @doc false
  def do_mutate(module, codon_index, left, right) do
    case ActiveMutation.mutation_index_for_codon(module, codon_index) do
      {:ok, 0} -> left && right
      {:ok, 1} -> true
      {:ok, 2} -> false
      _ -> left || right
    end
  end
end
