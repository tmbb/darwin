defmodule Darwin.Mutator.Rewriters.ClauseRewriterWithoutVariableShadowing do
  alias Darwin.Mutator
  alias Darwin.Mutator.Context
  alias Darwin.Erlang
  alias Darwin.Erlang.AbstractCode
  alias Darwin.Mutator.Rewriters.PatternRewriter
  alias Darwin.Mutators.Common.RespectPatternRewritesMutator
  require Erlang

  @moduledoc false

  @doc """
  TODO write
  """
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

  # Templates must be made into external resources so that this module
  # is automatically recompiled when these files change
  @external_resource "lib/darwin/mutator/rewriters/templates/not_shadowed/clause_with_guards.hrl"
  @external_resource "lib/darwin/mutator/rewriters/templates/not_shadowed/clause_without_guards.hrl"

  # These clauses won't cause variables in the patterns to shadow other variables.
  # This is what you want if you are mutating clauses in:
  #
  #    * case statements - case statements don't create a new scope, unlike functions
  #    * named (top-level) functions - top-level functions do create a new scope,
  #      but it doesn't really matter because at the top level there aren't any
  #      variables to be shadowed.
  #
  # This code can't be used to mutate inline anonymous functions (`fun ... -> ... end.` in
  # Erlang and `fn ... -> end` in Elixir).
  # These functinos create a new variable scope and using this code will lead to bugs
  # in the mutated code.
  defp clause_with_guards_to_function(clause, ctx) do
    {:clause, _line, patterns, guards, body} = clause
    flattened_guards = List.flatten(guards)

    # We must store the list of old mutators so that we can reset them
    old_mutators = ctx.mutators
    # We add a special mutator to the list.
    # This mutator recognizes equalities (in Erlang: `A == B`; in Elixir: `a == b`)
    # created by the `PatternRewriter` module (which is *not* a mutator!)
    # The `PatternRewriter` module generates expressions with a specific (very) high
    # line number, which won't occur in "normal" files.
    # The `RespectPatternRewritesMutator` will recognize those equalities
    # and will protect the `:==` operator from mutation.
    # That is, the `:==` mutator won't bew replaces by `:!=`, `:>`, etc.
    # Only the arguments will be mutated.
    new_mutators = [RespectPatternRewritesMutator | old_mutators]
    # Now, update the mutators in the context for the next few lines.
    # This will preserve some equalities in guards.
    ctx = Context.set_mutators(ctx, new_mutators)
    {mutated_guards, ctx} = Mutator.do_map_mutate(flattened_guards, ctx)
    # Reset the mutators (the body is not treated in a special way)
    ctx = Context.set_mutators(ctx, old_mutators)
    # Now we're back into the old mutators

    {mutated_body, ctx} = Mutator.do_mutate(body, ctx)
    # Now, create an abstract code expression which behaves the same way
    # as matching the guards (while beeing much slower naturally)
    guards_match = all_guards_match(mutated_guards)

    # We will need to turn the arguments into a tupple so that we can
    # always use a 1-argument function (having to handle functions with
    # different arities would be much more complex)
    pattern_tuple = {:tuple, 0, patterns}
    # Encase the body into a block because some things in Erlang don't play well
    # with ebing inside other things and a block can go pretty much everywhere.
    block = {:block, 0, mutated_body}

    # Create a dummy argument that wll be used to match a tuple.
    # Why don't we match the tuple directly?
    # We do it this way because matching the tuple directly may lead to
    # variable shadowing, which causes incorrect behaviour in case clauses.
    # By avoiding variable shadowing, this works both with top-level functions
    # and with case clauses.
    # It DOESN'T work with anonymous functions.
    dummy_argument = AbstractCode.unshadowable_variable()

    # Generating complex Erlang AST is quite tiring and error-prone.
    # Fortunately, we can create Erlang abstract code by parsing
    # the string representation of the code.
    # After having the abstract code, we can use it as a template and
    # replace parts of the code by other fragments of abstract code.
    # The `Erlang.interpolate_in_abstract_code!/2` is a macro that
    # generates Erlang abstract code from a string (at compile-time, of course)
    # and replaces a number of variables in the abstract code by the
    # given fragments of abstract code given (at runtime).
    # This doesn't allow us to replace everything (some things can't take
    # the same place as a variable), but it's quite convenient.
    mutated_abstract_code =
      Erlang.interpolate_in_abstract_code!(
        # Read from a file so that we can have proper syntax highlighting in the Erlang code.
        File.read!("lib/darwin/mutator/rewriters/templates/not_shadowed/clause_with_guards.hrl"),
        DummyArgument: dummy_argument,
        GuardsMatch: guards_match,
        PatternTuple: pattern_tuple,
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

    dummy_argument = AbstractCode.unshadowable_variable()

    mutated_abstract_code =
      Erlang.interpolate_in_abstract_code!(
        File.read!(
          "lib/darwin/mutator/rewriters/templates/not_shadowed/clause_without_guards.hrl"
        ),
        DummyArgument: dummy_argument,
        PatternTuple: pattern_tuple,
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
end
