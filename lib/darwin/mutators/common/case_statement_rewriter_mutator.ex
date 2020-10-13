defmodule Darwin.Mutators.Common.CaseStatementRewriterMutator do
  @behaviour Darwin.Mutator
  alias Darwin.Erlang.AbstractCode
  alias Darwin.Mutator.Rewriters.ClauseRewriter
  require Darwin.Erlang, as: Erlang

  @impl true
  def mutate({:case, _line, _value, clauses} = abstract_code, ctx) do
    case ClauseRewriter.any_clause_contains_guards?(clauses) do
      true ->
        {mutated_function, ctx} =
          CaseStatementRewriter.rewrite_case_statement(abstract_code, ctx)

        {:ok, {mutated_function, ctx}}

      false ->
        :error
    end
  end

  def mutate(_abstract_code, _ctx), do: :error

  def rewrite_case_statement({:case, _line, value, clauses}, ctx) do
    # Convert the clauses into anonymous functions
    {functions, ctx} = ClauseRewriter.clauses_to_functions(clauses, ctx)
    # Encode the list of functions as a compile-time erlag list which we can splice
    # into the Erlang abstract code.
    function_list = AbstractCode.encode_list(functions)

    body =
      Erlang.interpolate_in_abstract_code!(
        """
        'Elixir.Darwin.Mutator.Rewriters.ClauseRewriter':execute_transformed_clauses(
          Functions,
          Value).
        """,
        Functions: function_list,
        Value: value
      )
    # Although this function will be invoked by a mutator, it's not a mutator itself,
    # so it doesn't return `{:ok, {abstract_code, ctx}}`.
    # That will be the job of this function's caller.
    {body, ctx}
  end
end
