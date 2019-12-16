defmodule Darwin.Mutators.Common.GuardRewriterMutator do
  @behaviour Darwin.Mutator
  alias Darwin.Mutator.GuardRewriter

  @impl true
  def mutate({:function, _line, _atom_name, _arity, clauses} = abstract_code, ctx) do
    case GuardRewriter.any_clause_contains_guards?(clauses) do
      true ->
        {mutated_function, ctx} = GuardRewriter.rewrite_function(abstract_code, ctx)
        {:ok, {mutated_function, ctx}}

      false ->
        :error
    end
  end

  def mutate(_abstract_code, _ctx), do: :error
end
