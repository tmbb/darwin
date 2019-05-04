defmodule Darwin.Mutator.Helpers do
  import Darwin.Mutator.Helpers.DefHelper, only: [defhelper: 3]
  alias Darwin.Mutator.Context

  def call_remote(module, name, arguments) do
    {:call, 0, {:remote, 0, {:atom, 0, module}, {:atom, 0, name}}, arguments}
  end

  def call(fun, args) do
    {:call, 0, fun, args}
  end

  def fun(clauses) do
    {:fun, 0, {:clauses, clauses}}
  end

  defhelper(:get_active_mutation, [], do: Darwin.ActiveMutation.get())
  defhelper(:strict_not, [expression], do: not expression)
  defhelper(:permissive_not, [expression], do: !expression)

  defhelper(:bool_permissive_and, [a, b], do: a && b)
  defhelper(:bool_permissive_or, [a, b], do: a || b)
  defhelper(:bool_permissive_not, [a], do: !a)

  defhelper(:bool_strict_and, [a, b], do: a and b)
  defhelper(:bool_strict_or, [a, b], do: a or b)
  defhelper(:bool_strict_not, [a], do: not a)

  defhelper(:arith_add, [a, b], do: a + b)
  defhelper(:arith_sub, [a, b], do: a - b)
  defhelper(:arith_mul, [a, b], do: a * b)
  defhelper(:arith_div, [a, b], do: a / b)

  defhelper(:relation_greater_than, [a, b], do: a > b)
  defhelper(:relation_greater_than_or_equal, [a, b], do: a >= b)
  defhelper(:relation_equal, [a, b], do: a == b)
  defhelper(:relation_not_equal, [a, b], do: a != b)
  defhelper(:relation_less_than_or_equal, [a, b], do: a <= b)
  defhelper(:relation_less_than, [a, b], do: a < b)

  def escape_literal(n) when is_integer(n), do: {:integer, 0, n}
  def escape_literal(f) when is_float(f), do: {:float, 0, f}
  def escape_literal(atom) when is_atom(atom), do: {:atom, 0, atom}

  def mutation_clause(id, expression) do
    {:clause, [generated: true, location: 0], [id], [], [expression]}
  end

  def ignored_var(line \\ 0) do
    {:var, line, :_}
  end

  def make_case(%{next_mutation_id: id} = ctx, meta, value, expressions, original) do
    n = length(expressions)

    clauses =
      for {expression, i} <- Enum.with_index(expressions, 1) do
        mutation_clause(escape_literal(id + i), expression)
      end ++
        [
          mutation_clause(ignored_var(), original)
        ]

    {{:case, meta, value, clauses}, %{ctx | next_mutation_id: id + n}}
  end
end
