defmodule Darwin.Mutators.Default.CharlistMutator do
  alias Darwin.Mutator.Context
  alias Darwin.Erlang.AbstractCode
  # alias Darwin.Mutator.DefMutator

  def runtime_replace_charlist(charlist) when charlist == [], do: 'Darwin was here!'
  def runtime_replace_charlist(_charlist), do: ''

  def abstract_code_replace_charlist({:string, line, charlist}) do
    new_charlist = runtime_replace_charlist(charlist)
    {:string, line, new_charlist}
  end

  def mutate({:string, line, _charlist} = abstract_code, %Context{} = ctx, _mutators) do
    codon = Context.new_codon(ctx, value: abstract_code)
    %{index: codon_index} = codon
    nr_of_mutations = Context.nr_of_mutations(ctx)

    mutator = __MODULE__

    mutated_abstract_code =
      AbstractCode.call_mfa(
        {__MODULE__, :__do_mutate__,
         [
           AbstractCode.encode_atom(mutator),
           AbstractCode.encode_integer(codon_index, line: line),
           AbstractCode.encode_atom(nr_of_mutations, line: line),
           abstract_code
         ]},
        line: line
      )

    mutation = [
      mutator: mutator,
      name: "replace charlist",
      mutated_abstract_code: abstract_code_replace_charlist(abstract_code)
    ]

    ctx = Context.add_mutation(ctx, codon, mutation)

    {:ok, {mutated_abstract_code, ctx}}
  end

  def mutate(_abstact_code, _ctx, _mutators), do: :error

  @doc false
  def __do_mutate__(module, codon_index, mutation_index, arg) do
    {active_module, active_codon_index, active_mutation_index} = Darwin.ActiveMutation.get()

    case module == active_module and codon_index == active_codon_index do
      true ->
        corrected_index = active_mutation_index - mutation_index

        case corrected_index do
          0 -> runtime_replace_charlist(arg)
          _ -> arg
        end

      false ->
        arg
    end
  end
end
