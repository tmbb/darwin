defmodule Darwin.Mutators.Default.OpAddMutator do
  alias Darwin.Mutator.Context
  alias Darwin.ActiveMutation
  require Darwin.Mutator, as: Mutator

  def mutate(abstract_code = {:op, line, :+, left, right}, ctx) do
    %{module: module} = ctx

    # TODO: convert erlang to Elixir
    elixir_left = left
    elixir_right = right

    {mutated_left, ctx} = Mutator.do_mutate(left, ctx)
    {mutated_right, ctx} = Mutator.do_mutate(right, ctx)

    {codon, ctx} = Context.new_codon(ctx, value: abstract_code)
    %{index: codon_index} = codon

    mutated_abstract_code =
      Mutator.call_mutator(
        {__MODULE__, :do_mutate},
        {module, codon_index},
        [mutated_left, mutated_right],
        line
      )

    mutation_replace_by_sub = [
      mutator: __MODULE__,
      name: "replace by -",
      mutated_abstract_code: %{
        erlang: {:op, line, :-, left, right},
        elixir: quote(do: unquote(elixir_left) - unquote(elixir_right))
      }
    ]

    mutation_replace_by_mul = [
      mutator: __MODULE__,
      name: "replace by *",
      mutated_abstract_code: %{
        erlang: {:op, line, :*, left, right},
        elixir: quote(do: unquote(elixir_left) * unquote(elixir_right))
      }
    ]

    mutation_replace_by_div = [
      mutator: __MODULE__,
      name: "replace by /",
      mutated_abstract_code: %{
        erlang: {:op, line, :/, left, right},
        elixir: quote(do: unquote(elixir_left) / unquote(elixir_right))
      }
    ]

    mutations = [
      mutation_replace_by_sub,
      mutation_replace_by_mul,
      mutation_replace_by_div
    ]

    ctx = Context.add_mutations(ctx, codon_index, mutations)

    {:ok, {mutated_abstract_code, ctx}}
  end

  def mutate(_abstract_code, _ctx), do: :error

  @doc false
  def do_mutate(module, codon_index, left, right) do
    case ActiveMutation.mutation_index_for_codon(module, codon_index) do
      {:ok, 0} -> left - right
      {:ok, 1} -> left * right
      {:ok, 2} -> left / right
      _ -> left + right
    end
  end
end