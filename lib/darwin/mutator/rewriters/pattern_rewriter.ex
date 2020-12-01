defmodule Darwin.Mutator.Rewriters.PatternRewriter do
  alias Darwin.Erlang.AbstractCodeTransformer
  alias Darwin.Erlang.AbstractCode

  @moduledoc false

  @doc """
  This function factors the constants in patterns of a clause out of the pattern
  and into the guards where they can be mutated in a later pass.

  For example:

      {:ok, value}

  Becomes something equivalent to:

      {placeholder__0, value} when placeholder__0 == :ok

  This is exactly equivalent and allows us to mutate the `:ok` atom in the gaurd,
  using the `Darwin.Mutator.Rewriters.ClauseRewriter` module.
  Mutating the patterns in directly is pretty much impossible
  (most changes create code that fails to compile).
  Mutating guards is much simpler.

  As of now, this function is non-deterministic.
  It generates placeholders with low collision risk.
  If there is a collision, variables may be shadowed, which can introduce
  very subtle bugs in the mutated output.

  TODO: refactor this so that the output is deterministic for better testing.
  It's a pain to test code with non-deterministic output, even if we fix the seed
  of the random number generator.
  Will have to take the `%Context{}` as an argument and store a global counter there.
  """
  def rewrite_clause({:clause, line, patterns, guards, body}) do
    # The patterns and guards are meant to be used in a function or in a case statement.
    # They are roughly equivalent except for some details regarding variable scope.
    # Those details are important, though, and they can lead to pretty subtle bugs
    # in the mutated output.
    # Fortunately, these details won't matter here.
    # The only thing tahat will change is the way we'll mutate the list of clauses
    # later in the pipeline
    {new_patterns, extra_guards} = replace_constants_in_patterns(patterns)
    # Flatten all guards into a list.
    # TODO: figure out what exactly a list of guards means in Erlang.
    # Even without understanding what a list of guards does, it doesn't
    # really matter because the next steps in the pipeline know how to turn
    # the list of guards into correct executable code (mutated but correct)
    all_guards = extra_guards ++ List.flatten(guards)
    # Now we're ready to build the updated clause for further processing.
    {:clause, line, new_patterns, all_guards, body}
  end

  # Generate a new variable and update the state bindings
  # ´TODO: refactor so that it takes a `%Context{}` as an argument and we
  # can do away with non-determinism.
  defp new_variable(state, value) do
    %{counter: counter, bindings: bindings} = state
    # Generate a non-deterministic placeholder with low chances of collision
    variable = AbstractCode.unshadowable_variable()
    binding = {variable, value}
    # Uupdate the state with the new bindings.
    # We don't need to update the counter now that we don't use it for the placeholder,
    # But let's keep it for now.
    new_state = %{counter: counter + 1, bindings: [binding | bindings]}
    # Return both the variable and the updated bindings.
    {variable, new_state}
  end

  defp replace_constants_in_pattern(pattern, state) do
    {new_pattern, new_state} =
      AbstractCodeTransformer.transform(
        pattern,
        state,
        &replace_constant/2
      )

    %{bindings: bindings} = new_state
    guards = bindings_to_guards(bindings)
    {new_pattern, guards}
  end

  defp replace_constants_in_patterns(patterns) do
    # TODO: ditch the state and keep only the bindings;
    # Refactor the function to take a context and use a global counter there.
    initial_state = %{bindings: [], counter: 0}

    {all_reversed_patterns, all_reversed_guards, _state} =
      Enum.reduce(
        patterns,
        # A 3-tuple containing the following:
        #   * the new patterns
        #   * the extra guards (which might be empty if no constants were replaced)
        #   * the state that contains the bindings for the placeholders
        {_new_patterns = [], _extra_guards = [], initial_state},
        fn pattern, {reversed_patterns, guards, state} ->
          {new_pattern, new_guards} = replace_constants_in_pattern(pattern, state)
          {[new_pattern | reversed_patterns], new_guards ++ guards, state}
        end
      )

    # Reverse the lists above.
    # Strictly speaking, we don't need to reverse the guards, but the output
    # is a little prettier with the guards reversed (as pretty as mutated code can be...)
    all_patterns = :lists.reverse(all_reversed_patterns)
    all_guards = :lists.reverse(all_reversed_guards)

    {all_patterns, all_guards}
  end

  # Match operator (e.g. `rhs = lhs`)
  defp replace_constant({:match, _line, _lhs, _rhs} = match, state) do
    {:cont, match, state}
  end

  # Containers
  defp replace_constant({:tuple, _line, _elements} = tuple, state) do
    {:cont, tuple, state}
  end

  # The abstract code `{:cons, _line, head, tail}` is equivalent to `[head | tail]`
  defp replace_constant({:cons, _line, _head, _tail} = cons, state) do
    {:cont, cons, state}
  end

  # Constants
  defp replace_constant({:atom, _line, _name} = constant, state) do
    {variable, new_state} = new_variable(state, constant)
    {:halt, variable, new_state}
  end

  defp replace_constant({:integer, _line, _name} = constant, state) do
    {variable, new_state} = new_variable(state, constant)
    {:halt, variable, new_state}
  end

  defp replace_constant({:float, _line, _name} = constant, state) do
    {variable, new_state} = new_variable(state, constant)
    {:halt, variable, new_state}
  end

  # Everything else causes us to stop processing
  defp replace_constant(other, state) do
    {:halt, other, state}
  end

  defp bindings_to_guards(bindings) do
    for {variable, value} <- bindings do
      AbstractCode.encode_equals(
        variable,
        value,
        line: 1_000_000_000_000
      )
    end
  end

  def f({p__0, 1} = {a, b}) when p__0 == :ok, do: a + b
end
