defmodule Darwin.Mutators.Default.OpLessThanOrEqualToMutator do
  @behaviour Darwin.Mutator
  alias Darwin.Mutator.Context
  alias Darwin.ActiveMutation
  alias Darwin.ErlToEx
  require Darwin.Mutator, as: Mutator

  @impl true
  def mutate(abstract_code = {:op, line, :"=<", left, right}, ctx) do
    %{module: module} = ctx

    # TODO: convert erlang to Elixir
    elixir_left = ErlToEx.erl_to_ex(left)
    elixir_right = ErlToEx.erl_to_ex(right)

    {mutated_left, ctx} = Mutator.do_mutate(left, ctx)
    {mutated_right, ctx} = Mutator.do_mutate(right, ctx)

    {codon, ctx} = Context.new_codon(ctx, value: abstract_code, line: line)
    %{index: codon_index} = codon

    mutated_abstract_code =
      Mutator.mutation_for_codon(
        {__MODULE__, :darwin_was_here},
        {module, codon_index},
        [mutated_left, mutated_right],
        line
      )

    mutation_replace_by_less_than = [
      mutator: __MODULE__,
      name: "replace by <",
      mutated_codon: %{
        erlang: {:op, line, :<, left, right},
        elixir: quote(do: unquote(elixir_left) < unquote(elixir_right))
      }
    ]

    mutation_replace_by_equal_to = [
      mutator: __MODULE__,
      name: "replace by ==",
      mutated_codon: %{
        erlang: {:op, line, :==, left, right},
        elixir: quote(do: unquote(elixir_left) == unquote(elixir_right))
      }
    ]

    mutation_replace_by_not_equal_to = [
      mutator: __MODULE__,
      name: "replace by !=",
      mutated_codon: %{
        erlang: {:op, line, :"/=", left, right},
        elixir: quote(do: unquote(elixir_left) != unquote(elixir_right))
      }
    ]

    mutation_replace_by_greater_than_or_equal_to = [
      mutator: __MODULE__,
      name: "replace by >=",
      mutated_codon: %{
        erlang: {:op, line, :>=, left, right},
        elixir: quote(do: unquote(elixir_left) >= unquote(elixir_right))
      }
    ]

    mutation_replace_by_greater_than = [
      mutator: __MODULE__,
      name: "replace by >",
      mutated_codon: %{
        erlang: {:op, line, :>, left, right},
        elixir: quote(do: unquote(elixir_left) > unquote(elixir_right))
      }
    ]

    mutation_replace_by_true = [
      mutator: __MODULE__,
      name: "replace by 'true'",
      mutated_codon: %{
        erlang: {:atom, line, true},
        elixir: quote(do: true)
      }
    ]

    mutation_replace_by_false = [
      mutator: __MODULE__,
      name: "replace by 'false'",
      mutated_codon: %{
        erlang: {:atom, line, false},
        elixir: quote(do: false)
      }
    ]

    mutations = [
      mutation_replace_by_less_than,
      mutation_replace_by_equal_to,
      mutation_replace_by_not_equal_to,
      mutation_replace_by_greater_than,
      mutation_replace_by_greater_than_or_equal_to,
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
      {:ok, 0} -> left < right
      {:ok, 1} -> left == right
      {:ok, 2} -> left != right
      {:ok, 3} -> left >= right
      {:ok, 4} -> left > right
      {:ok, 5} -> true
      {:ok, 6} -> false
      _ -> left < right
    end
  end
end
