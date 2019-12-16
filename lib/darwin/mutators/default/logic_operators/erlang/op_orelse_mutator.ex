defmodule Darwin.Mutators.Default.OpOrelseMutator do
  @behaviour Darwin.Mutator
  alias Darwin.Mutator.Context
  alias Darwin.ActiveMutation
  alias Darwin.ErlToEx
  require Darwin.Mutator, as: Mutator

  # TODO: ADD-TESTS

  @impl true
  def mutate(abstract_code = {:op, line, :orelse, left, right}, ctx) do
    %{module: module} = ctx

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

    mutation_replace_by_and = [
      mutator: __MODULE__,
      name: "replace by 'and'",
      mutated_codon: %{
        erlang: {:op, line, :orelse, left, right},
        elixir: quote(do: unquote(elixir_left) and unquote(elixir_right))
      }
    ]

    mutation_replace_by_true = [
      mutator: __MODULE__,
      name: "replace by 'true'",
      mutated_codon: %{
        erlang: {:atom, 0, true},
        elixir: quote(do: true)
      }
    ]

    mutation_replace_by_false = [
      mutator: __MODULE__,
      name: "replace by 'false'",
      mutated_codon: %{
        erlang: {:atom, 0, false},
        elixir: quote(do: false)
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
  def darwin_was_here(module, codon_index, left, right) do
    case ActiveMutation.mutation_index_for_codon(module, codon_index) do
      {:ok, 0} -> left and right
      {:ok, 1} -> true
      {:ok, 2} -> false
      _ -> :erlang.orelse(left, right)
    end
  end
end
