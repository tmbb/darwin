defmodule Darwin.Mutator.Helpers.DefMutator do
  alias Darwin.Mutation

  @doc """
  Apply the mutators in order until one of them matches.

  A mutator `m` matches if `m.mutate/3` returns an {:ok, {ast, ctx}} tuple.
  """
  def apply_mutators(mutators, ast, ctx) do
    Enum.reduce_while(mutators, {ast, ctx}, fn mutator, {ast, ctx} ->
      case mutator.mutate(ast, ctx, mutators) do
        {:ok, {new_ast, new_ctx}} -> {:halt, {new_ast, new_ctx}}
        :error -> {:cont, {ast, ctx}}
      end
    end)
  end

  defp make_case_clauses(default, alternatives, args) do
    clauses =
      for {alternative, index} <- Enum.with_index(alternatives, 0) do
        fun = Keyword.fetch!(alternative, :f)

        quote do
          unquote(index) -> apply(unquote(fun), unquote(args))
        end
      end
      |> List.flatten()

    default_clause =
      quote do
        _ -> apply(unquote(default), unquote(args))
      end

    clauses ++ default_clause
  end

  @debug true
  @debug_filename "debug_mutators.exs"

  def debug_def_bin_op_mutator(quoted) do
    contents =
      quoted
      |> Macro.to_string()
      |> Code.format_string!(locals_without_parens: [def: :*])

    if File.exists?(@debug_filename) do
      File.write!(@debug_filename, ["\n\n", contents], [:append])
    else
      File.write!(@debug_filename, contents)
    end
  end

  @doc """
  Defines a function to mutate a node in the Erlang abstract code.
  """
  defmacro def_bin_op_mutator(name, op, default, alternatives) do
    # Name for the helper function
    helper_name = :"__do_#{name}__"

    mutations =
      for alternative <- alternatives do
        # TODO: refactor the %Mutation{} struct
        type = op
        message = Keyword.fetch!(alternative, :text)
        Mutation.new(type: type, message: message)
      end

    # Programatically generate new variables so that they can be used in the case statement
    # and in the quoted expression beloww
    left = Macro.var(:left, __MODULE__)
    right = Macro.var(:right, __MODULE__)
    # Now, we generate the case clauses.
    case_clauses = make_case_clauses(default, alternatives, [left, right])
    # The first argument is missing because we'll pipe it inside the quoted expression.
    # It will be easier to keep proper hygiene if we pipe it there instead of generating
    # another variable outside the quoted expression.
    case_statement = {:case, [], [[do: case_clauses]]}

    # We need the caller module so that we can insert the fully namespaced call to the
    # `__do_#{name}__` function into the Erlang abstract code
    caller_module = __CALLER__.module

    function_definitions =
      quote do
        # This function is the one that will mutate the Erlang abstract code.
        # It does the following:
        #
        # 1. It mutates the operator arguments (by recursively calling `apply_mutators()`)
        # 2. It replaces the AST node with a function call to the `__do_#{name}__` function
        #
        # The `__do_#{name}__` is the one called during the tests, and the one responsible for
        # picking which code to run (i.e., each branch of a case statement according to the active mutation)
        def unquote(name)(
              {:op, line, unquote(op), left, right} = _abstract_code,
              %Darwin.Mutator.Context{} = ctx,
              mutators
            ) do
          alias Darwin.Mutator.Helpers.DefMutator
          alias Darwin.ErlUtils.AbstractCode
          alias Darwin.Mutator.Context

          # Mutate the operands (updating the context, of course)
          {mutated_left, ctx} = DefMutator.apply_mutators(mutators, left, ctx)
          {mutated_right, ctx} = DefMutator.apply_mutators(mutators, right, ctx)

          count = ctx.count
          module = ctx.module

          ast =
            AbstractCode.call_mfa(
              {unquote(caller_module), unquote(helper_name),
               [
                 AbstractCode.encode_atom(module),
                 AbstractCode.encode_integer(count, line),
                 mutated_left,
                 mutated_right
               ]},
              line
            )

          new_ctx = Context.add_mutations(ctx, unquote(Macro.escape(mutations)))

          {:ok, {ast, new_ctx}}
        end

        def unquote(name)(_ast, _ctx, _mutators) do
          :error
        end

        @doc false
        # This function will be used during testing (i.e. after the Erlang abstract code has been mutated).
        # It encapsulates the whole logic of retrieving the active mutation and executing
        # the correct branch according to the picked mutation.
        def unquote(helper_name)(module, line, start, unquote(left), unquote(right)) do
          # The current mutation is identified as a pair containing a module name and a number.
          # This allows us to use numbers to identify mutations *in the same module*
          # and disambiguate between different modules using the module name.
          #
          # Why not use just the number?
          # Because if we allow different modules to contain the same number,
          # we can mutate modules in parallel.
          # Mutating modules in parallel is not possible if we are restricted
          # to using a single stream of consecutive numbers.
          {active_module, active_mutation_nr} = Darwin.ActiveMutation.get()
          # We will match on the difference between the current mutation number
          # and the starting mutation number.
          # That way, the indices in the case statement can always start at zero.
          # This reduces the amount of code generation we must do.
          corrected_index = active_mutation_nr - start
          # Are we mutating this module?
          case module == active_module do
            # If we are in the correct module, we test whether the corrected index matches
            # the active mutation and run the matching alternative
            true ->
              # The case statement matches the correct index agains the alternatives
              # (see above for the definition of the case statement)
              corrected_index |> unquote(case_statement)

            # If we aren't in the correct module, we run the default code
            # (we also run the default code in the last clause of the case statement)
            false ->
              apply(unquote(default), [unquote(left), unquote(right)])
          end
        end
      end

    if @debug do
      debug_def_bin_op_mutator(function_definitions)
    end

    # Finally, return the quoted expression
    function_definitions
  end
end
