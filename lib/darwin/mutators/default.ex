defmodule Darwin.Mutators.Default.BinOpMutatorCreator do
  @moduledoc false

  defp build_table(mutations, left_header, right_header) do
    pairs =
      for mutation <- mutations do
        {"`#{mutation.example_input}`", "`#{mutation.example_output}`"}
      end

    {left, right} = Enum.unzip(pairs)
    left_max_length = [left_header | left] |> Enum.map(&String.length/1) |> Enum.max()
    right_max_length = [right_header | right] |> Enum.map(&String.length/1) |> Enum.max()

    padded_left = Enum.map(left, fn cell -> String.pad_trailing(cell, left_max_length) end)
    padded_right = Enum.map(right, fn cell -> String.pad_trailing(cell, right_max_length) end)
    padded_left_header = String.pad_trailing(left_header, left_max_length)
    padded_right_header = String.pad_trailing(right_header, right_max_length)

    left_separator = String.duplicate("-", left_max_length)
    right_separator = String.duplicate("-", right_max_length)
    cells = Enum.zip(padded_left, padded_right)

    %{
      left_header: padded_left_header,
      left_separator: left_separator,
      right_header: padded_right_header,
      right_separator: right_separator,
      cells: cells
    }
  end

  def create_mutator(prefix, mutator) do
    # Get the full module name
    module = Module.concat(prefix, mutator.suffix)
    # Create the mutations for the operator, according to the rules below
    mutations = mutations_for_binary_operator(mutator.operator, mutator.default)
    # Build table for docstring
    table = build_table(mutations, "Original", "Mutated")

    docstring =
      EEx.eval_string(
        """
        Mutates the `<%= mutator.operator %>` operator.

        ### Available mutations:

        | <%= table.left_header %> | <%= table.right_header %> |
        | <%= table.left_separator %> | <%= table.right_separator %> |<%= for {left, right} <- table.cells do %>
        | <%= left %> | <%= right%> |<% end %>
        """,
        table: table,
        mutator: mutator
      )

    alternatives =
      for mutation <- mutations do
        f = mutation.f
        description = mutation.description
        text = "#{description} (e.g. `#{mutation.example_input}` -> `#{mutation.example_output}`)"

        [
          f: f,
          text: text
        ]
      end

    contents =
      quote do
        @moduledoc unquote(docstring)
        require Darwin.Mutator.Helpers.DefMutator, as: DefMutator

        DefMutator.def_bin_op_mutator(
          :mutate,
          unquote(mutator.operator),
          unquote(mutator.default),
          unquote(alternatives)
        )
      end

    Module.create(module, contents, Macro.Env.location(__ENV__))
  end

  @comparison_operators [:<, :<=, :==, :!=, :>=, :>]
  @arithmetic_operators [:+, :-, :*, :/]
  @commutative_operators [:+, :*, :==, :!=]

  defguard is_commutative(op) when op in @commutative_operators
  defguard is_arithmetic_operator(op) when op in @arithmetic_operators
  defguard is_comparison_operator(op) when op in @comparison_operators

  # Mutations for operators are the following:
  # - switch order of arguments
  # - delete arguments (only for arithmetic operators)
  # - replace by arithmetic operator (only for arithmetic operators)
  # - replace by comparison operator (only for comparison operators)

  def maybe_switch_order_of_arguments(op, fun) when not is_commutative(op) do
    [
      %{
        f: quote(do: fn a, b -> unquote(fun).(b, a) end),
        description: "'#{op}' operator: switched order of arguments",
        example_input: "a #{op} b",
        example_output: "b #{op} a"
      }
    ]
  end

  def maybe_switch_order_of_arguments(_op, _fun), do: []

  def maybe_delete_arguments(op, _fun) when is_arithmetic_operator(op) do
    [
      %{
        f: quote(do: fn a, _b -> a end),
        description: "'#{op}' operator: delete right argument",
        example_input: "a #{op} b",
        example_output: "a"
      },
      %{
        f: quote(do: fn _a, b -> b end),
        description: "'#{op}' operator: delete left argument",
        example_input: "a #{op} b",
        example_output: "b"
      }
    ]
  end

  def maybe_delete_arguments(_op, _fun), do: []

  def maybe_replace_by_arithmetic_operator(op, _fun) when is_arithmetic_operator(op) do
    ops_and_funs = [
      {:+, quote(do: &Kernel.+/2)},
      {:-, quote(do: &Kernel.-/2)},
      {:*, quote(do: &Kernel.*/2)},
      {:/, quote(do: &Kernel.//2)}
    ]

    for {other_op, other_fun} <- ops_and_funs, other_op != op do
      %{
        f: quote(do: fn a, b -> unquote(other_fun).(b, a) end),
        description: "replace '#{op}' by '#{other_op}'",
        example_input: "a #{op} b",
        example_output: "b #{other_op} a"
      }
    end
  end

  def maybe_replace_by_arithmetic_operator(_op, _fun), do: []

  def maybe_replace_by_comparison_operator(op, _fun) when is_comparison_operator(op) do
    ops_and_funs = [
      {:<, quote(do: &Kernel.</2)},
      {:<=, quote(do: &Kernel.<=/2)},
      {:==, quote(do: &Kernel.==/2)},
      {:!=, quote(do: &Kernel.!=/2)},
      {:>=, quote(do: &Kernel.>=/2)},
      {:>, quote(do: &Kernel.>/2)}
    ]

    for {other_op, other_fun} <- ops_and_funs, other_op != op do
      %{
        f: quote(do: fn a, b -> unquote(other_fun).(b, a) end),
        description: "replace '#{op}' by '#{other_op}'",
        example_input: "a #{op} b",
        example_output: "b #{other_op} a"
      }
    end
  end

  def maybe_replace_by_comparison_operator(_op, _fun), do: []

  def maybe_replace_by_true_or_false(op, _fun) when op in @comparison_operators do
    [
      %{
        f: quote(do: fn _a, _b -> false end),
        description: "replace '#{op}' by comparison that always fails",
        example_input: "a #{op} b",
        example_output: "false"
      },
      %{
        f: quote(do: fn _a, _b -> true end),
        description: "replace '#{op}' by comparison that always succeeds",
        example_input: "a #{op} b",
        example_output: "true"
      }
    ]
  end

  def maybe_replace_by_true_or_false(_op, _fun), do: []

  def mutations_for_binary_operator(op, fun) do
    switch_order = maybe_switch_order_of_arguments(op, fun)
    replace_by_arithmetic_operator = maybe_replace_by_arithmetic_operator(op, fun)
    replace_by_comparison_operator = maybe_replace_by_comparison_operator(op, fun)
    delete_arguments = maybe_delete_arguments(op, fun)
    replace_by_true_or_false = maybe_replace_by_true_or_false(op, fun)

    Enum.concat([
      switch_order,
      replace_by_arithmetic_operator,
      replace_by_comparison_operator,
      delete_arguments,
      replace_by_true_or_false
    ])
  end
end

defmodule Darwin.Mutators.Default do
  alias Darwin.Mutators.Default.{
    OpAddMutator,
    OpSubMutator,
    OpMulMutator,
    OpDivMutator,
    OpLessThanMutator,
    OpLessThanOrEqualToMutator,
    OpEqualToMutator,
    OpNotEqualToMutator,
    OpGreaterThanMutator,
    OpGreaterThanOrEqualToMutator,
    OpGreaterThanMutator
  }

  def mutators() do
    [
      OpAddMutator,
      OpSubMutator,
      OpMulMutator,
      OpDivMutator,
      OpLessThanMutator,
      OpLessThanOrEqualToMutator,
      OpEqualToMutator,
      OpNotEqualToMutator,
      OpGreaterThanMutator,
      OpGreaterThanOrEqualToMutator,
      OpGreaterThanMutator
    ]
  end
end

arithmetic_operator_mutators = [
  %{
    suffix: OpAddMutator,
    operator: :+,
    default: quote(do: &Kernel.+/2)
  },
  %{
    suffix: OpSubMutator,
    operator: :-,
    default: quote(do: &Kernel.-/2)
  },
  %{
    suffix: OpMulMutator,
    operator: :*,
    default: quote(do: &Kernel.*/2)
  },
  %{
    suffix: OpDivMutator,
    operator: :/,
    default: quote(do: &Kernel.//2)
  }
]

comparison_operator_mutators = [
  %{
    suffix: OpLessThanMutator,
    operator: :<,
    default: quote(do: &Kernel.</2)
  },
  %{
    suffix: OpLessThanOrEqualToMutator,
    operator: :<=,
    default: quote(do: &Kernel.<=/2)
  },
  %{
    suffix: OpEqualToMutator,
    operator: :==,
    default: quote(do: &Kernel.==/2)
  },
  %{
    suffix: OpNotEqualToMutator,
    operator: :!=,
    default: quote(do: &Kernel.!=/2)
  },
  %{
    suffix: OpGreaterThanOrEqualToMutator,
    operator: :>=,
    default: quote(do: &Kernel.>=/2)
  },
  %{
    suffix: OpGreaterThanMutator,
    operator: :>,
    default: quote(do: &Kernel.>/2)
  }
]

mutators = arithmetic_operator_mutators ++ comparison_operator_mutators

alias Darwin.Mutators.Default.BinOpMutatorCreator

for mutator <- mutators do
  BinOpMutatorCreator.create_mutator(Darwin.Mutators.Default, mutator)
end
