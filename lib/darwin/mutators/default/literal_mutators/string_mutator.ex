defmodule Darwin.Mutators.Default.StringMutator do
  @behaviour Darwin.Mutator
  @moduledoc """
  Mutates a static string (or *binary*, in erlang)
  """
  alias Darwin.Mutator.Context
  alias Darwin.ActiveMutation
  alias Darwin.Mutators.Default.CharlistMutator
  alias Darwin.Mutator

  @impl true
  def mutate(
        {:bin, line1, [{:bin_element, _line2, {:string, _line3, charlist}, :default, :default}]} =
          abstract_code,
        %Context{} = ctx
      )
      when is_list(charlist) do
    %{module: module} = ctx
    {codon, ctx} = Context.new_codon(ctx, value: abstract_code, line: line1)
    %{index: codon_index} = codon

    mutated_abstract_code =
      Mutator.mutation_for_codon(
        {__MODULE__, :darwin_was_here},
        {module, codon_index},
        [abstract_code],
        line1
      )

    mutation_replace_string = [
      mutator: __MODULE__,
      name: "replace string",
      mutated_codon: %{
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
  def darwin_was_here(module, codon_index, arg) do
    case ActiveMutation.mutation_index_for_codon(module, codon_index) do
      {:ok, 0} -> runtime_replace_string(arg)
      _ -> arg
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
