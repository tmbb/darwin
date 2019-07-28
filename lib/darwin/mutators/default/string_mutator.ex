defmodule Darwin.Mutators.Default.StringMutator do
  @moduledoc """
  Mutates a static string (or *binary*, in erlang)
  """
  alias Darwin.Mutator.Context
  alias Darwin.Erlang.AbstractCode
  alias Darwin.Mutators.Default.CharlistMutator

  defp runtime_replace_string(string) when string == "", do: "Darwin was here!"
  defp runtime_replace_string(_string), do: ""

  defp abstract_code_replace_string(
         {:bin, line1, [{:bin_element, line2, {:string, line3, charlist}, :default, :default}]}
       )
       when is_list(charlist) do
    new_charlist = CharlistMutator.runtime_replace_charlist(charlist)
    {:bin, line1, [{:bin_element, line2, {:string, line3, new_charlist}, :default, :default}]}
  end

  def mutate(
        {:bin, line1, [{:bin_element, _line2, {:string, _line3, charlist}, :default, :default}]} =
          abstract_code,
        %Context{} = ctx,
        _mutators
      )
      when is_list(charlist) do
    codon = Context.new_codon(ctx, value: abstract_code, line: line1)
    %{index: codon_index} = codon
    nr_of_mutations = Context.nr_of_mutations(ctx)

    mutator = __MODULE__

    mutated_abstract_code =
      AbstractCode.call_mfa(
        {__MODULE__, :__do_mutate__,
         [
           AbstractCode.encode_atom(mutator),
           AbstractCode.encode_integer(codon_index, line: line1),
           AbstractCode.encode_atom(nr_of_mutations, line: line1),
           abstract_code
         ]},
        line: line1
      )

    mutated_pattern = abstract_code_replace_string(abstract_code)

    mutation = [
      mutator: mutator,
      name: "replace charlist",
      mutated_abstract_code: mutated_pattern
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
          0 -> arg |> runtime_replace_string() |> to_string()
          _ -> arg
        end

      false ->
        to_string(arg)
    end
  end
end
