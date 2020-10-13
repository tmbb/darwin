defmodule Darwin.Mutators.Common.FunctionDefinitionRewriterMutator do
  @behaviour Darwin.Mutator
  alias Darwin.Erlang.AbstractCode
  alias Darwin.Mutator.Rewriters.ClauseRewriter
  require Darwin.Erlang, as: Erlang

  @impl true
  def mutate({:function, _line, _atom_name, _arity, clauses} = abstract_code, ctx) do
    case ClauseRewriter.any_clause_contains_guards?(clauses) do
      true ->
        {mutated_function, ctx} =
          rewrite_function(abstract_code, ctx)

        {:ok, {mutated_function, ctx}}

      false ->
        :error
    end
  end

  def mutate(_abstract_code, _ctx), do: :error

  # Generate dummy arguments for a function with arity `arity`.
  # These arguments are "raw" variables because they must match everything.
  defp dummy_arguments(arity) do
    for i <- 1..arity do
      {:var, 0, :"X#{i}_@darwin"}
    end
  end

  def rewrite_function({:function, line, f, arity, clauses}, ctx) do
    # A function with several clauses will be rewritten into a function
    # with a single clause.
    #
    # The single clause must accept any arguments that respect the arity,
    # and the pattern and guard matching will be done in the body if this function
    # We need to create new arguments for this function, which what we do with
    #
    # the `dummy_aruments/1` function.
    # There arguments are normal variables, which will match everything,
    # just like we wanted.
    arguments = dummy_arguments(arity)
    arguments_tuple = AbstractCode.encode_tuple(arguments)
    # Convert the clauses into anonymous functions
    {functions, ctx} = ClauseRewriter.clauses_to_functions(clauses, ctx)
    # Encode the list of functions as a compile-time erlag list which we can splice
    # into the Erlang abstract code.
    function_list = AbstractCode.encode_list(functions)

    body =
      Erlang.interpolate_in_abstract_code!(
        """
        Clauses_@darwin = Functions,
        'Elixir.Darwin.Mutator.Rewriters.ClauseRewriter':execute_transformed_clauses(
          Clauses_@darwin,
          ArgumentsTuple).
        """,
        Functions: function_list,
        ArgumentsTuple: arguments_tuple
      )

    # We can now build the single clause for the mutated function.
    clause = {:clause, line, arguments, [], body}

    # And we can also build the full function definition for the mutated function
    mutated_function = {
      :function,
      line,
      f,
      arity,
      [clause]
    }

    # Although this function will be invoked by a mutator, it's not a mutator itself,
    # so it doesn't return `{:ok, {abstract_code, ctx}}`.
    # That will be the job of this function's caller.
    {mutated_function, ctx}
  end
end
