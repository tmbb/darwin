defmodule Darwin.Mutator do
  alias Darwin.Mutator.Context
  alias Darwin.Mutators.Default
  alias Darwin.Erlang.AbstractCode

  @type mutator_result() :: {:ok, {AbstractCode.t(), Context.t()}} | :error

  @type mutator() :: atom()

  @callback mutate(AbstractCode.t(), Context.t(), list()) :: nil

  @doc """
  Tests whether a clause in the Erlang abstract code is generated.
  """
  defguard is_generated(clause)
           when is_tuple(clause) and hd(elem(clause, 1)) == {:generated, true}

  @doc """
  Tests whether a list of clauses are generated.
  """
  defmacro are_generated([clause | clauses]) do
    initial = quote(do: Darwin.Mutator.is_generated(unquote(clause)))

    Enum.reduce(clauses, initial, fn clause, conjunction ->
      quote do
        Darwin.Mutator.is_generated(unquote(clause)) and unquote(conjunction)
      end
    end)
  end

  def call_mutator(
        {caller_module, helper_name} = _fun,
        {module, codon_index} = _codon,
        args,
        line
      ) do
    codon_args = [
      AbstractCode.encode_atom(module, line: 0),
      AbstractCode.encode_integer(codon_index, line: 0)
    ]

    AbstractCode.call_mfa(
      {caller_module, helper_name, codon_args ++ args},
      line: line
    )
  end

  @doc """
  Mutates Erlang abstract code.
  """
  @spec mutate(AbstractCode.t(), atom(), list(mutator())) :: mutator_result()
  def mutate(abstract_code, module, mutators \\ Default.mutators()) do
    ctx = Context.new(module: module, mutators: mutators)
    do_mutate(abstract_code, ctx)
  end

  @doc """
  Apply the mutators in order until one of them matches.

  A mutator `m` matches if `m.mutate/3` returns an {:ok, {abstract_code, ctx}} tuple.
  """
  @spec mutate(AbstractCode.t(), atom(), Context.t()) :: mutator_result()
  def do_mutate(abstract_code, ctx) do
    %{mutators: mutators} = ctx

    Enum.reduce_while(mutators, {abstract_code, ctx}, fn mutator, {abstract_code, ctx} ->
      case mutator.mutate(abstract_code, ctx) do
        {:ok, {new_abstract_code, new_ctx}} -> {:halt, {new_abstract_code, new_ctx}}
        :error -> {:cont, {abstract_code, ctx}}
      end
    end)
  end

  def do_map_mutate(list, ctx) do
    {mutated_reversed_list, ctx} =
      Enum.reduce(list, {[], ctx}, fn abstract_code, {acc, ctx} ->
        {mutated_abstract_code, ctx} = do_mutate(abstract_code, ctx)
        {[mutated_abstract_code | acc], ctx}
      end)

    {:lists.reverse(mutated_reversed_list), ctx}
  end

  @doc false
  def make_mutation_opts(abstract_code, mutator, mutation) do
    {m, f, args} = Keyword.fetch!(mutation, :abstract_code_mfa)
    mutated_abstract_code = apply(m, f, args ++ [abstract_code])

    mutation
    |> Keyword.put(:mutated_codon, mutated_abstract_code)
    |> Keyword.put(:mutator, mutator)
  end

  defp make_case_clauses(default_mfa, alternatives, op_args) do
    clauses =
      for {alternative, index} <- Enum.with_index(alternatives, 0) do
        quoted_mfa = Keyword.fetch!(alternative, :runtime_mfa)

        quote do
          unquote(index) ->
            {alt_m, alt_f, alt_args} = unquote(quoted_mfa)
            apply(alt_m, alt_f, alt_args ++ unquote(op_args))
        end
      end
      |> List.flatten()

    default_clause =
      quote do
        _ ->
          {default_m, default_f, default_args} = unquote(default_mfa)
          apply(default_m, default_f, default_args ++ unquote(op_args))
      end

    clauses ++ default_clause
  end

  @debug true
  @debug_filename "debug_mutators.exs"

  defp debug_def_bin_op_mutator(quoted) do
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
  Defines a function to mutate a binary operator in the Erlang abstract code.
  """
  defmacro def_bin_op_mutator(name, op, default_mfa, alternatives) do
    # Name for the helper function
    helper_name = :"__do_#{name}__"

    # Programatically generate new variables so that they can be used in the case statement
    # and in the quoted expression beloww
    left = Macro.var(:left, __MODULE__)
    right = Macro.var(:right, __MODULE__)
    # Now, we generate the case clauses.
    case_clauses = make_case_clauses(default_mfa, alternatives, [left, right])
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
        # 1. It mutates the operator arguments (by recursively calling `do_mutate()`)
        # 2. It replaces the AST node with a function call to the `__do_#{name}__` function
        #
        # The `__do_#{name}__` is the one called during the tests, and the one responsible for
        # picking which code to run (i.e., each branch of a case statement according to the active mutation)
        def unquote(name)(
              {:op, line, unquote(op), left, right} = abstract_code,
              %Darwin.Mutator.Context{} = ctx,
              mutators
            ) do
          # Alias the modules locally
          alias Darwin.Erlang.AbstractCode
          alias Darwin.Mutator.Context
          alias Darwin.Mutator

          # Mutate the operands (updating the context, of course)
          {mutated_left, ctx} = Mutator.do_mutate(mutators, left, ctx)
          {mutated_right, ctx} = Mutator.do_mutate(mutators, right, ctx)

          {codon, ctx} = Context.new_codon(ctx, value: abstract_code)
          %{index: codon_index} = codon
          nr_of_mutations = Context.nr_of_mutations(ctx)
          module = ctx.module

          mutated_abstract_code =
            Mutator.call_mutator(
              {__MODULE__, :do_mutate},
              {module, codon_index},
              [mutated_left, mutated_right],
              line
            )

          # mutated_abstract_code =
          #   AbstractCode.call_mfa(
          #     {unquote(caller_module), unquote(helper_name),
          #      [
          #        AbstractCode.encode_atom(module),
          #        AbstractCode.encode_integer(codon_index, line: line),
          #        AbstractCode.encode_integer(nr_of_mutations, line: line),
          #        mutated_left,
          #        mutated_right
          #      ]},
          #     line: line
          #   )

          mutation_data = unquote(alternatives)

          mutations =
            Enum.map(mutation_data, fn mutation ->
              Mutator.make_mutation_opts(
                abstract_code,
                unquote(caller_module),
                mutation
              )
            end)

          ctx = Context.add_mutations(ctx, codon, mutations)

          {:ok, {mutated_abstract_code, ctx}}
        end

        def unquote(name)(_abstract, _ctx, _mutators) do
          :error
        end

        @doc false
        # This function will be used during testing (i.e. after the Erlang abstract code has been mutated).
        # It encapsulates the whole logic of retrieving the active mutation and executing
        # the correct branch according to the picked mutation.
        def unquote(helper_name)(
              module,
              codon_index,
              mutation_index,
              unquote(left),
              unquote(right)
            ) do
          # The current mutation is identified as a pair containing a module name and a number.
          # This allows us to use numbers to identify mutations *in the same module*
          # and disambiguate between different modules using the module name.
          #
          # Why not use just the number?
          # Because if we allow different modules to contain the same number,
          # we can mutate modules in parallel.
          # Mutating modules in parallel is not possible if we are restricted
          # to using a single stream of consecutive numbers.
          {active_module, active_codon_index, active_mutation_index} = Darwin.ActiveMutation.get()
          # Are we mutating this module and this codon?
          case module == active_module and codon_index == active_codon_index do
            # If we are in the correct module, we test whether the corrected index matches
            # the active mutation and run the matching alternative
            true ->
              # We will match on the difference between the current mutation number
              # and the starting mutation number.
              # That way, the indices in the case statement can always start at zero.
              # This reduces the amount of code generation we must do.
              corrected_index = active_mutation_index - mutation_index
              # The case statement matches the correct index agains the alternatives
              # (see above for the definition of the case statement)
              corrected_index |> unquote(case_statement)

            # If we aren't in the correct module, we run the default code
            # (we also run the default code in the last clause of the case statement)
            false ->
              {m, f, args} = unquote(default_mfa)
              apply(m, f, args ++ [unquote(left), unquote(right)])
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
