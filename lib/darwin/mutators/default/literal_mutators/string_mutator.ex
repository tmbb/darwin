defmodule Darwin.Mutators.Default.StringMutator do
  @moduledoc """
  Mutates a static string (or *binary*, in erlang)
  """
  alias Darwin.Mutator.Context
  alias Darwin.ActiveMutation
  alias Darwin.Mutators.Default.CharlistMutator
  alias Darwin.Mutator

  def mutate(
        {:bin, line1, [{:bin_element, _line2, {:string, _line3, charlist}, :default, :default}]} =
          abstract_code,
        %Context{} = ctx
      )
      when is_list(charlist) do
    %{module: module} = ctx
    {codon, ctx} = Context.new_codon(ctx, value: abstract_code)
    %{index: codon_index} = codon

    mutated_abstract_code =
      Mutator.call_mutator(
        {__MODULE__, :do_mutate},
        {module, codon_index},
        [abstract_code],
        line1
      )

    mutation_replace_string = [
      mutator: __MODULE__,
      name: "replace string",
      mutated_abstract_code: %{
        elixir: runtime_replace_string(charlist),
        erlang: abstract_code_replace_string(abstract_code)
      }
    ]

    mutations = [
      mutation_replace_string
    ]

    ctx = Context.add_mutations(ctx, codon_index, mutations)

    {:ok, {mutated_abstract_code, ctx}}
  end

  def mutate(_abstract_code, _ctx), do: :error

  @doc false
  def do_mutate(module, codon_index, arg) do
    case ActiveMutation.mutation_index_for_codon(module, codon_index) do
      {:ok, 0} -> runtime_replace_string(arg)
      _ -> not arg
    end
  end

  defp runtime_replace_string(charlist) when charlist == '', do: "Darwin was here!"
  defp runtime_replace_string(_charlist), do: ""

  defp abstract_code_replace_string(
         {:bin, line1, [{:bin_element, line2, {:string, line3, charlist}, :default, :default}]}
       )
       when is_list(charlist) do
    new_charlist = CharlistMutator.runtime_replace_charlist(charlist)
    {:bin, line1, [{:bin_element, line2, {:string, line3, new_charlist}, :default, :default}]}
  end
end
