defmodule Darwin.Mutators.Default.OpLaxNotMutator do
  @behaviour Darwin.Mutator
  alias Darwin.Mutator.Context
  alias Darwin.ActiveMutation
  alias Darwin.ErlToEx
  require Darwin.Mutator, as: Mutator

  def mutate(
        abstract_code =
          {:case, line, arg,
           [
             {:clause, _, [{:var, _, atom1}],
              [
                [
                  {:op, _, :orelse, {:op, _, :"=:=", {:var, _, atom2}, {:atom, _, nil}},
                   {:op, _, :"=:=", {:var, _, atom3}, {:atom, _, false}}}
                ]
              ], [{:atom, _, true}]},
             {:clause, _, [{:var, _, :_}], [], [{:atom, _, false}]}
           ]},
        ctx
      )
      when atom1 == atom2 and atom2 == atom3 do
    {codon, ctx} = Context.new_codon(ctx, value: abstract_code, line: line)
    %{module: module} = ctx
    %{index: codon_index} = codon

    elixir_arg = ErlToEx.erl_to_ex(arg)

    {mutated_arg, ctx} = Mutator.do_mutate(arg, ctx)

    mutated_abstract_code =
      Mutator.mutation_for_codon(
        {__MODULE__, :darwin_was_here},
        {module, codon_index},
        [mutated_arg],
        line
      )

    mutation_remove_negation = [
      mutator: __MODULE__,
      name: "remove negation (!)",
      mutated_codon: %{
        elixir: quote(do: unquote(elixir_arg)),
        erlang: arg
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
      mutation_remove_negation,
      mutation_replace_by_true,
      mutation_replace_by_false
    ]

    ctx = Context.add_mutations(ctx, codon_index, mutations)

    {:ok, {mutated_abstract_code, ctx}}
  end

  def mutate(_abstract_code, _ctx), do: :error

  @doc false
  def darwin_was_here(module, codon_index, arg) do
    case ActiveMutation.mutation_index_for_codon(module, codon_index) do
      {:ok, 0} -> arg
      {:ok, 1} -> true
      {:ok, 2} -> false
      _ -> !arg
    end
  end
end
