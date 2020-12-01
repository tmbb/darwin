defmodule Darwin.Mutator.Rewriters.CaseClauseRewriter do
  alias Darwin.Mutator
  alias Darwin.Mutator.Context
  alias Darwin.Erlang
  alias Darwin.Erlang.AbstractCode
  alias Darwin.Mutator.Rewriters.PatternRewriter
  alias Darwin.Mutators.Common.RespectPatternRewritesMutator
  require Erlang

  @moduledoc false

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
        "lists:all(fun (X) -> X end, AllGuards).",
        AllGuards: guard_list
      )

    expression
  end

  @doc false
  def execute_transformed_function_clauses([], _arg_tuple) do
    raise FunctionClauseError
  end

  def execute_transformed_function_clauses([clause | clauses], arg_tuple) do
    case clause.(arg_tuple) do
      {:ok, result} ->
        result

      :error ->
        execute_transformed_function_clauses(clauses, arg_tuple)
    end
  end

  @doc false
  def execute_transformed_case_clauses([], _arg_tuple) do
    raise CaseClauseError
  end

  def execute_transformed_case_clauses([clause | clauses], arg_tuple) do
    case clause.(arg_tuple) do
      {:ok, result} ->
        result

      :error ->
        execute_transformed_case_clauses(clauses, arg_tuple)
    end
  end

  defp clause_with_guards_to_function(clause, ctx) do
    {:clause, _line, patterns, guards, body} = clause
    flattened_guards = List.flatten(guards)

    # We must store the list of old mutators so that we can reset them
    old_mutators = ctx.mutators
    new_mutators = [RespectPatternRewritesMutator | old_mutators]
    # Now, update the mutators in the context for the next few lines.
    # This will preserve some equalities in guards.
    ctx = Context.set_mutators(ctx, new_mutators)
    {mutated_guards, ctx} = Mutator.do_map_mutate(flattened_guards, ctx)
    # Reset the mutators (the body is not treated in a special way)
    ctx = Context.set_mutators(ctx, old_mutators)

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

  defp clause_without_guards_to_function(clause, ctx) do
    {:clause, _line, patterns, _guards, body} = clause

    # Mutate the body
    {mutated_body, ctx} = Mutator.do_mutate(body, ctx)

    pattern_tuple = {:tuple, 0, patterns}
    block = {:block, 0, mutated_body}

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

  defp clause_to_function(clause, ctx) do
    clause_with_rewritten_patterns = PatternRewriter.rewrite_clause(clause)
    case clause_has_guards?(clause_with_rewritten_patterns) do
      true ->
        clause_with_guards_to_function(clause_with_rewritten_patterns, ctx)

      false ->
        clause_without_guards_to_function(clause_with_rewritten_patterns, ctx)
    end
  end

  defp clause_has_guards?(clause) do
    {:clause, _line, _patterns, guards, _body} = clause
    List.flatten(guards) != []
  end

  # Code based on the `Darwin.Mutator.do_map_mutate/2` function
  def clauses_to_functions(clauses, ctx) do
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
end
