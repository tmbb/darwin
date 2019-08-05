# defmodule Darwin.Mutators.Default.BitstringMutator do
#   @moduledoc
#   alias Darwin.Mutator.Context
#   alias Darwin.Erlang.AbstractCode
#   alias Darwin.Mutator.DefMutator

#   @doc false
#   def runtime_replace_charlist(string) when string == "", do: "Darwin was here!"
#   def runtime_replace_charlist(_string), do: ""

#   defp abstract_code_replace_charlist(
#          {:bin, line1, [{:bin_element, line2, {:string, line3, charlist}, :default, :default}]}
#        )
#        when is_list(charlist) do
#     new_charlist = CharlistMutator.runtime_replace_charlist(charlist)
#     {:bin, line1, [{:bin_element, line2, {:string, line3, new_charlist}, :default, :default}]}
#   end

#   def mutate(
#         {:bin, line, patterns} = abstract_code,
#         %Context{} = ctx
#       )
#       when is_list(patterns) do
#     {reversed_mutated_patterns, ctx} =
#       Enum.reduce(patterns, {[], ctx}, fn pattern, {mutated_patterns, ctx} ->
#         {abs_code, ctx} = DefMutator.do_mutate(pattern, ctx, mutators)
#         {[abs_code | mutated_patterns], ctx}
#       end)

#     mutated_patterns = :lists.reverse(reversed_mutated_patterns)

#     codon = Context.new_codon(ctx, value: abstract_code, line: line)
#     %{index: codon_index} = codon
#     nr_of_mutations = Context.nr_of_mutations(ctx)

#     mutator = __MODULE__

#     mutated_bitstring = {:bin, line, mutated_patterns}

#     mutated_abstract_code =
#       AbstractCode.call_mfa(
#         {__MODULE__, :do_mutate,
#          [
#            AbstractCode.encode_atom(mutator),
#            AbstractCode.encode_integer(codon_index, line: line),
#            AbstractCode.encode_atom(nr_of_mutations, line: line),
#            mutated_bitstring
#          ]},
#         line: line
#       )

#     mutated_charlist = CharlistMutator.runtime_replace_charlist(charlist)

#     mutation = [
#       mutator: mutator,
#       name: "replace charlist",
#       mutated_abstract_code: mutated_pattern
#     ]

#     ctx = Context.add_mutation(ctx, codon, mutation)

#     {:ok, {mutated_abstract_code, ctx}}
#   end

#   def mutate(_abstact_code, _ctx), do: :error

#   @doc false
#   def do_mutate(module, codon_index, mutation_index, arg) do
#     {active_module, active_codon_index, active_mutation_index} = Darwin.ActiveMutation.get()

#     case module == active_module and codon_index == active_codon_index do
#       true ->
#         corrected_index = active_mutation_index - mutation_index

#         case corrected_index do
#           0 -> arg |> runtime_replace_string() |> to_string()
#           _ -> arg
#         end

#       false ->
#         to_string(arg)
#     end
#   end
# end
