defmodule Darwin do
  require Logger

  def get_abst(module) do
    filename = "_build/dev/lib/darwin/ebin/" <> Atom.to_string(module) <> ".beam"
    filename_charlist = String.to_charlist(filename)

    {:ok, {_, [{:abstract_code, {_, abs_code}}]}} =
      :beam_lib.chunks(filename_charlist, [:abstract_code])

    _reconstructed =
      abs_code
      |> :erl_syntax.form_list()
      |> :erl_prettypr.format()
      |> to_string()

    :beam_disasm.file(filename_charlist)
  end

  # Arithmetic operator replacement (AOR)
  # Logical connector replacement (LCR)
  # Relational operator replacement (ROR)
  # Unary operator insertion (UOR)
  # Statement block removal (SBR)

  @strict_binary_boolean_operators [
    :or,
    :and
  ]

  @permissive_binary_boolean_operators [
    :||,
    :&&
  ]

  # @comparison_operators [
  #   :==,
  #   :!=,
  #   :>,
  #   :>=,
  #   :<,
  #   :<=
  # ]

  @arithmetic_operators [
    :+,
    :-,
    :*,
    :/
  ]

  @integer_mutations [
    -1,
    0,
    1,
    2
  ]

  @float_mutations [
    -1.0,
    0.0,
    1.0,
    1.4
  ]

  @boolean_mutations [
    nil,
    true,
    false
  ]

  def fragment_for_operator_mutation(
        index,
        alternatives,
        {operator, meta, _args} = _original
      ) do
    mutations =
      Enum.filter(alternatives, fn atom ->
        atom != operator
      end)

    var_a = Macro.var(:a, __MODULE__)
    var_b = Macro.var(:b, __MODULE__)

    clauses_for_mutated_code =
      for {op, i} <- Enum.with_index(mutations, 0) do
        quote do
          unquote(index + i) ->
            # Should we use the `meta` context here?
            # Or should we use the empty list instead?
            unquote({op, meta, [var_a, var_b]})
        end
      end

    clause_for_original_code =
      quote do
        _other ->
          unquote({operator, meta, [var_a, var_b]})
      end

    clauses = List.flatten(clauses_for_mutated_code ++ [clause_for_original_code])

    fragment =
      quote do
        fn unquote(var_a), unquote(var_b) ->
          unquote({:case, [], [quote(do: Darwin.ActiveMutation.get()), [do: clauses]]})
        end
      end

    new_index = index + length(mutations)
    {fragment, new_index}
  end

  def binary_operator_mutation(
        index,
        alternatives,
        _constants,
        {_op, meta, [left, right]} = original
      ) do
    {fragment, index} = fragment_for_operator_mutation(index, alternatives, original)
    {mutated_left, index} = mutations_for(index, left)
    {mutated_right, index} = mutations_for(index, right)
    call = {:., [], [fragment]}
    result = {call, meta, [mutated_left, mutated_right]}

    {result, index}
  end

  def if_statement_mutation(index, {:if, meta, [condition, branches]} = _if_statement) do
    new_condition =
      quote do
        case Darwin.ActiveMutation.get() do
          unquote(index) -> true
          unquote(index + 1) -> false
          unquote(index + 2) -> !unquote(condition)
          _other -> unquote(condition)
        end
      end

    new_if_statement = {:if, meta, [new_condition, branches]}

    {new_if_statement, index + 3}
  end

  def literal_mutation(index, original, alternatives) do
    mutations = List.delete(alternatives, original)

    clause_for_original_code =
      quote do
        _other -> unquote(original)
      end

    clauses_for_mutated_code =
      for {mutation, i} <- Enum.with_index(mutations, 0) do
        quote do
          unquote(index + i) -> unquote(mutation)
        end
      end

    clauses = List.flatten(clauses_for_mutated_code ++ [clause_for_original_code])

    fragment = {:case, [], [quote(do: Darwin.ActiveMutation.get()), [do: clauses]]}

    new_index = index + length(mutations)
    {fragment, new_index}
  end

  def mutations_for(index, {op, _meta, [_left, _right]} = original)
      when op in @arithmetic_operators do
    binary_operator_mutation(index, @arithmetic_operators, [], original)
  end

  def mutations_for(index, {op, _meta, [_left, _right]} = original)
      when op in @strict_binary_boolean_operators do
    binary_operator_mutation(index, @strict_binary_boolean_operators, [], original)
  end

  def mutations_for(index, {op, _meta, [_left, _right]} = original)
      when op in @permissive_binary_boolean_operators do
    binary_operator_mutation(index, @permissive_binary_boolean_operators, [], original)
  end

  def mutations_for(index, {:if, _meta, [_condition, _branches]} = if_statement) do
    if_statement_mutation(index, if_statement)
  end

  def mutations_for(index, n) when is_integer(n) do
    literal_mutation(index, n, @integer_mutations)
  end

  def mutations_for(index, f) when is_float(f) do
    literal_mutation(index, f, @float_mutations)
  end

  def mutations_for(index, b) when b in @boolean_mutations do
    literal_mutation(index, b, @boolean_mutations)
  end

  def mutations_for(index, other), do: {other, index}

  def mutate(expression) do
    {mutated, _index} = mutations_for(0, expression)

    Logger.debug(fn ->
      debug =
        mutated
        |> Macro.to_string()
        |> Code.format_string!()

      ["\n", debug]
    end)

    mutated
  end

  def format(expression) do
    expression
    |> Macro.to_string()
    |> Code.format_string!()
    |> Kernel.++(["\n"])
    |> List.to_string()
  end

  # def perform_strict_boolean_operator_replacement(index, {op, _meta, [_left, _right]} = original)
  #     when op in @strict_boolean_binary_operators do
  #   binary_operator_mutation(index, @strict_boolean_binary_operators, original)
  # end
end
