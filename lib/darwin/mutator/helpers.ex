defmodule Darwin.Mutator.Helpers do
  import Darwin.Mutator.Helpers.DefHelper, only: [defhelper: 3]

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

  defhelper(:relational_greater_than, [a, b], do: a > b)
  defhelper(:relational_greater_than_or_equal, [a, b], do: a >= b)
  defhelper(:relational_equal, [a, b], do: a == b)
  defhelper(:relational_not_equal, [a, b], do: a != b)
  defhelper(:relational_less_than_or_equal, [a, b], do: a <= b)
  defhelper(:relational_less_than, [a, b], do: a < b)

  def escape_literal(n) when is_integer(n), do: {:integer, 0, n}
  def escape_literal(f) when is_float(f), do: {:float, 0, f}
  def escape_literal(atom) when is_atom(atom), do: {:atom, 0, atom}

  def tuple(args), do: {:tuple, 0, args}
  def atom(arg) when is_atom(arg), do: {:atom, 0, arg}
  def integer(arg) when is_integer(arg), do: {:integer, 0, arg}

  defp mutation_clause(module, mut_id, expression) do
    {:clause, [generated: true, location: 0],
     [
       tuple([atom(module), integer(mut_id)])
     ], [], [expression]}
  end

  defp default_clause(expression) do
    {:clause, [generated: true, location: 0], [ignored_var()], [], [expression]}
  end

  def ignored_var(line \\ 0) do
    {:var, line, :_}
  end

  def make_case(module, value, branches, original) do
    clauses =
      for {mut_id, right} <- branches do
        mutation_clause(module, mut_id, right)
      end ++
        [
          default_clause(original)
        ]

    {:case, 0, value, clauses}
  end
end
