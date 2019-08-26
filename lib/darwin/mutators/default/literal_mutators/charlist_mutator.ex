defmodule Darwin.Mutators.Default.CharlistMutator do
  alias Darwin.Mutator.Context
  alias Darwin.Mutator
  alias Darwin.ActiveMutation

  def mutate({:string, line, charlist} = abstract_code, %Context{} = ctx) do
    %{module: module} = ctx
    {codon, ctx} = Context.new_codon(ctx, value: abstract_code, line: line)
    %{index: codon_index} = codon

    mutated_abstract_code =
      Mutator.call_mutator(
        {__MODULE__, :do_mutate},
        {module, codon_index},
        [abstract_code],
        line
      )

    mutation_replace_charlist = [
      mutator: __MODULE__,
      name: "replace charlist",
      mutated_codon: %{
        elixir: runtime_replace_charlist(charlist),
        erlang: abstract_code_replace_charlist(abstract_code)
      }
    ]

    mutations = [
      mutation_replace_charlist
    ]

    ctx = Context.add_mutations(ctx, codon_index, mutations)

    {:ok, {mutated_abstract_code, ctx}}
  end

  def mutate(_abstract_code, _ctx), do: :error

  @doc false
  def do_mutate(module, codon_index, arg) do
    case ActiveMutation.mutation_index_for_codon(module, codon_index) do
      {:ok, 0} -> runtime_replace_charlist(arg)
      _ -> arg
    end
  end

  @doc false
  def runtime_replace_charlist(charlist) when charlist == [], do: 'Darwin was here!'
  def runtime_replace_charlist(_charlist), do: ''

  defp abstract_code_replace_charlist({:string, line, charlist}) do
    new_charlist = runtime_replace_charlist(charlist)
    {:string, line, new_charlist}
  end
end
