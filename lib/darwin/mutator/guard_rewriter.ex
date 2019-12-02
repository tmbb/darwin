defmodule Darwin.Mutator.GuardRewriter do
  alias Darwin.Mutator
  alias Darwin.Erlang
  alias Darwin.Erlang.AbstractCode
  require Erlang

  @doc false

  # This module is private, and the correct way to mutate guards and clauses
  # is through the `Mutators.Default.Common.GuardRewriterMutator`.

  defp clause_contains_guards?({:clause, _line, _patterns, guards, _body}) do
    List.flatten(guards) != []
  end

  def any_clause_contains_guards?(clauses) do
    Enum.any?(clauses, &clause_contains_guards?/1)
  end

  # If there is only a single guard, just output it; noo need to overcomplicate
  defp all_guards_match([guard]) do
    guard
  end

  # In all other cases, we'll match them using the `:lists.all/1` function.
  defp all_guards_match(flattened_guards) do
    guard_list = AbstractCode.encode_list(flattened_guards)

    [expression] =
      Erlang.interpolate_in_abstract_code!(
        "lists:all(fun (x) -> x end, AllGuards).",
        AllGuards: guard_list
      )

    expression
  end

  @doc false
  def execute_transformed_clauses([], _arg_tuple) do
    raise(FunctionClauseError, "no clause matched")
  end

  def execute_transformed_clauses([clause | clauses], arg_tuple) do
    case clause.(arg_tuple) do
      {:ok, result} ->
        result

      :error ->
        execute_transformed_clauses(clauses, arg_tuple)
    end
  end

  def clause_with_guards_to_function(clause, ctx) do
    {:clause, _line, patterns, guards, body} = clause
    flattened_guards = List.flatten(guards)

    {mutated_guards, ctx} = Mutator.do_map_mutate(flattened_guards, ctx)
    {mutated_body, ctx} = Mutator.do_mutate(body, ctx)

    guards_match = all_guards_match(mutated_guards)

    pattern_tuple = {:tuple, 0, patterns}
    block = {:block, 0, mutated_body}

    mutated_abstract_code =
      Erlang.interpolate_in_abstract_code!(
        """
        fun (Arguments) ->
          case (try GuardsMatch
                catch
                  error:_ -> false
                end) of
            true -> {ok, Body};
            false -> error
          end;
            (_) -> error
        end.
        """,
        GuardsMatch: guards_match,
        Arguments: pattern_tuple,
        Body: block
      )

    {mutated_abstract_code, ctx}
  end

  def clause_without_guards_to_function(clause, ctx) do
    {:clause, _line, patterns, _guards, body} = clause

    pattern_tuple = {:tuple, 0, patterns}
    block = {:block, 0, body}

    mutated_abstract_code =
      Erlang.interpolate_in_abstract_code!(
        """
        fun (Arguments) ->
            {ok, Body};
            (_) -> error
        end.
        """,
        Arguments: pattern_tuple,
        Body: block
      )

    {mutated_abstract_code, ctx}
  end

  def clause_to_function(clause, ctx) do
    case clause_has_guards?(clause) do
      true -> clause_with_guards_to_function(clause, ctx)
      false -> clause_without_guards_to_function(clause, ctx)
    end
  end

  defp clause_has_guards?(clause) do
    {:clause, _line, _patterns, guards, _body} = clause
    List.flatten(guards) != []
  end

  # Generate dummy arguments for a function with arity `arity`.
  # These arguments are "raw" variables because they must match everything.
  def dummy_arguments(arity) do
    for i <- 1..arity do
      {:var, 0, :"X#{i}_@darwin"}
    end
  end

  # Code based on the `Darwin.Mutator.do_map_mutate/2` function
  defp clauses_to_functions(clauses, ctx) do
    {mutated_reversed_list_of_clauses, ctx} =
      Enum.reduce(clauses, {[], ctx}, fn clause, {acc, ctx} ->
        {mutated_abstract_code, ctx} = clause_to_function(clause, ctx)
        {[mutated_abstract_code | acc], ctx}
      end)

    mutated_clauses =
      mutated_reversed_list_of_clauses
      |> Enum.reverse()
      |> List.flatten()

    {mutated_clauses, ctx}
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
    {functions, ctx} = clauses_to_functions(clauses, ctx)
    # Encode the list of functions as a compile-time erlag list which we can splice
    # into the Erlang abstract code.
    function_list = AbstractCode.encode_list(functions)

    body =
      Erlang.interpolate_in_abstract_code!(
        """
        Clauses_@darwin = Functions,
        'Elixir.Darwin.Mutator.GuardRewriter':execute_transformed_clauses(
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
