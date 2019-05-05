defmodule Darwin.Mutator do
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
  # @strict_logical_connector_atoms []

  # @strict_logical_connector_constants [
  #   true,
  #   false
  # ]

  # @strict_logical_connectors []

  # Relational operator replacement (ROR)
  @relational_operator_atoms [
    :>,
    :>=,
    :==,
    :"/=",
    :<=,
    :<
  ]

  @relational_operator_constants [
    true,
    false
  ]

  @relational_operators [
    {:>, {Helpers, :relational_greater_than}},
    {:>=, {Helpers, :relational_greater_than_or_equal}},
    {:==, {Helpers, :relational_equal}},
    {:"/=", {Helpers, :relational_not_equal}},
    {:<=, {Helpers, :relational_less_than_or_equal}},
    {:<, {Helpers, :relational_less_than}}
  ]

  # Unary operator insertion (UOR)

  # Statement block removal (SBR)

  def line_number_from_meta(line_nr) when is_integer(line_nr), do: line_nr
  def line_number_from_meta(meta) when is_list(meta), do: Keyword.get(meta, :location)

  def mutate_binary_operator(%Context{} = ctx, alternatives, constants, operator, left, right) do
    mutations = Enum.filter(alternatives, fn {op, _mod_fun} -> op != operator end)
    {_operator, {module, function}} = Enum.find(alternatives, fn {op, _} -> op == operator end)
    arg1 = {:var, 0, :_darwin_a@1}
    arg2 = {:var, 0, :_darwin_b@1}

    {mutated_left, ctx} = do_mutate_abstract_code(ctx, left)
    {mutated_right, ctx} = do_mutate_abstract_code(ctx, right)

    {constant_values, ctx} =
      Enum.map_reduce(constants, ctx, fn constant, ctx ->
        {mut_id, ctx} =
          Context.add_mutation(ctx, {:replace_operator_by_constant, {operator, constant}})

        {{mut_id, constant}, ctx}
      end)

    # for constant <- constants do
    #   Helpers.escape_literal(constant)
    # end

    {expressions, ctx} =
      Enum.map_reduce(mutations, ctx, fn {op, {mod, fun}}, ctx ->
        {mut_id, ctx} = Context.add_mutation(ctx, {:replace_operator, {operator, op}})
        branch_value = apply(mod, fun, [arg1, arg2])
        {{mut_id, branch_value}, ctx}
      end)

    IO.inspect(expressions)

    case_statement_clauses = constant_values ++ expressions
    original = apply(module, function, [arg1, arg2])

    case_statement =
      Helpers.make_case(
        ctx.module,
        Helpers.get_active_mutation(),
        case_statement_clauses,
        original
      )

    fun = Helpers.fun([{:clause, 0, [arg1, arg2], [], [case_statement]}])
    result = Helpers.call(fun, [mutated_left, mutated_right])

    {result, ctx}
  end

  def do_mutate_abstract_code(%Context{} = ctx, {:op, _meta, operator, left, right})
      when operator in @arithmetic_operator_atoms do
    mutate_binary_operator(ctx, @arithmetic_operators, [], operator, left, right)
  end

  def do_mutate_abstract_code(%Context{} = ctx, {:op, _meta, operator, left, right})
      when operator in @relational_operator_atoms do
    mutate_binary_operator(
      ctx,
      @relational_operators,
      @relational_operator_constants,
      operator,
      left,
      right
    )
  end

  def do_mutate_abstract_code(ctx, ast), do: {ast, ctx}

  def unpack_and(
        {:case, line_nr, arg_left,
         [
           {:clause, [generated: true, location: _], [{:atom, _, false}], [],
            [{:atom, 0, false}]},
           {:clause, [generated: true, location: _], [{:atom, _, true}], [], [arg_right]},
           {:clause, [generated: true, location: _], [{:var, _, var1}], [],
            [
              {:call, _, {:remote, _, {:atom, _, :erlang}, {:atom, _, :error}},
               [
                 {:tuple, _, [{:atom, _, :badbool}, {:atom, _, :and}, {:var, _, var2}]}
               ]}
            ]}
         ]}
      )
      when var1 == var2 do
    {:ok, {line_nr, {arg_left, arg_right}}}
  end

  def unpack_and(_other), do: :error

  def unpack_or(
        {:case, line_nr, arg_left,
         [
           {:clause, [generated: true, location: _], [{:atom, _, false}], [], [arg_right]},
           {:clause, [generated: true, location: _], [{:atom, _, true}], [], [{:atom, _, true}]},
           {:clause, [generated: true, location: _], [{:var, _, var1}], [],
            [
              {:call, _, {:remote, _, {:atom, _, :erlang}, {:atom, _, :error}},
               [
                 {:tuple, _, [{:atom, _, :badbool}, {:atom, _, :or}, {:var, _, var2}]}
               ]}
            ]}
         ]}
      )
      when var1 == var2 do
    {:ok, {line_nr, {arg_left, arg_right}}}
  end

  def unpack_or(_other), do: :error

  def mutate_abstract_code(input_ast, context \\ nil) do
    ctx = context || Context.new()
    {output_ast, _ctx} = do_mutate_abstract_code(ctx, input_ast)

    Logger.debug(fn -> log_mutation(input_ast, output_ast) end)

    output_ast
  end

  def mutate_module(module) do
    ctx = Context.new(module: module)
    abstract_code = abstract_code(module)
    do_mutate_abstract_code(ctx, abstract_code)
  end

  def mutate_and_compile_module(module) do
    {abstract_code, ctx} = mutate_module(module)
    compiled = :compile.forms(abstract_code)

    result =
      case compiled do
        {:ok, ^module, _binary} -> {:ok, module}
        :error -> :error
      end

    {result, ctx}
  end

  def mutate_and_compile_modules(modules) do
    mutate_and_compile_modules__async(modules)
  end

  defp mutate_and_compile_modules__async(modules) do
    modules
    |> Task.async_stream(&mutate_and_compile_module/1)
    |> Enum.to_list()
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
      "Darwin.mutate_abstract_code
      /1",
      "\n>>> Input:\n",
      input,
      "\n\n>>> Output:\n",
      output,
      "\n"
    ]
  end
end
