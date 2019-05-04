defmodule Darwin do
  require Logger
  alias Darwin.ErlUtils
  alias Darwin.Mutator.Context
  alias Darwin.Mutator.Helpers

  # Arithmetic operator replacement (AOR)
  @arithmetic_operator_atoms [
    :+,
    :-,
    :*,
    :/
  ]

  @arithmetic_operators [
    {:+, {Helpers, :arith_add}},
    {:-, {Helpers, :arith_sub}},
    {:*, {Helpers, :arith_mul}},
    {:/, {Helpers, :arith_div}}
  ]
  # Logical connector replacement (LCR)

  # Relational operator replacement (ROR)
  @logical_connector_atoms [
    :>,
    :>=,
    :==,
    :"/=",
    :<=,
    :<
  ]

  # @logical_connector_constants [
  #   true,
  #   false,
  #   nil
  # ]

  @logical_connectors [
    {:>, {Helpers, :relation_greater_than}},
    {:>=, {Helpers, :relation_greater_than}},
    {:==, {Helpers, :relation_greater_than}},
    {:"/=", {Helpers, :relation_greater_than}},
    {:<=, {Helpers, :relation_greater_than}},
    {:<, {Helpers, :relation_greater_than}}
  ]

  # Unary operator insertion (UOR)

  # Statement block removal (SBR)

  def line_number_from_meta(line_nr) when is_integer(line_nr), do: line_nr
  def line_number_from_meta(meta) when is_list(meta), do: Keyword.get(meta, :location)

  def mutate_binary_operator(ctx, alternatives, operator, left, right) do
    mutations = Enum.filter(alternatives, fn {op, _mod_fun} -> op != operator end)
    {_operator, {module, function}} = Enum.find(alternatives, fn {op, _} -> op == operator end)
    arg1 = {:var, 0, :_darwin_a@1}
    arg2 = {:var, 0, :_darwin_b@1}

    expressions =
      for {_, {mod, fun}} <- mutations do
        apply(mod, fun, [arg1, arg2])
      end

    original = apply(module, function, [arg1, arg2])

    {case_statement, ctx} =
      Helpers.make_case(ctx, 0, Helpers.get_active_mutation(), expressions, original)

    fun = Helpers.fun([{:clause, 0, [arg1, arg2], [], [case_statement]}])
    result = Helpers.call(fun, [left, right])

    {result, ctx}
  end

  def do_mutate(%Context{} = ctx, {:op, _meta, operator, left, right})
      when operator in @arithmetic_operator_atoms do
    mutate_binary_operator(ctx, @arithmetic_operators, operator, left, right)
  end

  def do_mutate(%Context{} = ctx, {:op, _meta, operator, left, right})
      when operator in @logical_connector_atoms do
    mutate_binary_operator(ctx, @logical_connectors, operator, left, right)
  end

  def do_mutate(ctx, ast) do
    {ast, ctx}
  end

  def mutate(input_ast) do
    ctx = Context.new()
    {output_ast, _ctx} = do_mutate(ctx, input_ast)

    Logger.debug(fn -> log_mutation(input_ast, output_ast) end)

    output_ast
  end

  def abstract_code(module) do
    file = :code.which(module)
    result = :beam_lib.chunks(file, [:abstract_code])
    {:ok, {_, [{:abstract_code, {_, abstract_code}}]}} = result
    abstract_code
  end

  defp log_mutation(input_ast, output_ast) do
    input = ErlUtils.pprint(input_ast)
    output = ErlUtils.pprint(output_ast)

    [
      "Darwin.mutate/1",
      "\n>>> Input:\n",
      input,
      "\n\n>>> Output:\n",
      output,
      "\n"
    ]
  end
end
