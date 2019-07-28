defmodule Darwin.Mutators.Default.BinOpMutatorCreator do
  @moduledoc false

  defp build_table(mutations, left_header, right_header) do
    pairs =
      for mutation <- mutations do
        example_input = mutation[:example_input]
        example_output = mutation[:example_output]

        {"`#{example_input}`", "`#{example_output}`"}
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
    mutator_suffix = Keyword.fetch!(mutator, :suffix)
    mutator_operator = Keyword.fetch!(mutator, :operator)
    mutator_runtime_mfa = Keyword.fetch!(mutator, :runtime_mfa)
    mutator_abstract_code_mfa = Keyword.fetch!(mutator, :abstract_code_mfa)

    module = Module.concat(prefix, mutator_suffix)
    # Create the mutations for the operator, according to the rules below
    mutations =
      mutations_for_binary_operator(
        mutator_operator,
        mutator_runtime_mfa,
        mutator_abstract_code_mfa
      )

    # Build table for docstring
    table = build_table(mutations, "Original", "Mutated")

    docstring =
      EEx.eval_string(
        """
        Mutates the `<%= operator %>` operator.

        ### Available mutations:

        | <%= table.left_header %> | <%= table.right_header %> |
        | <%= table.left_separator %> | <%= table.right_separator %> |<%= for {left, right} <- table.cells do %>
        | <%= left %> | <%= right%> |<% end %>
        """,
        table: table,
        operator: mutator_operator
      )

    contents =
      quote do
        @moduledoc unquote(docstring)
        require Darwin.Mutators, as: Mutators

        Mutators.def_bin_op_mutator(
          :mutate,
          unquote(mutator_operator),
          unquote(Macro.escape(mutator_runtime_mfa)),
          unquote(Macro.escape(mutations))
        )
      end

    Module.create(module, contents, Macro.Env.location(__ENV__))
  end

  # ----------------
  # Common Functions
  # ----------------

  @doc false
  def runtime_delete_left(_mfa, a, _b), do: a
  @doc false
  def runtime_delete_right(_mfa, _a, b), do: b
  @doc false
  def runtime_swap_args({m, f, args}, a, b), do: apply(m, f, args ++ [b, a])

  @doc false
  def abstract_code_swap_args({:op, line, op, a, b}), do: {:op, line, op, b, a}
  @doc false
  def abstract_code_delete_left({:op, _line, _op, _a, b}), do: b
  @doc false
  def abstract_code_delete_right({:op, _line, _op, a, _b}), do: a

  @commutative_operators [:+, :*, :==, :!=]

  defguard is_commutative(op) when op in @commutative_operators

  # -----------------------------
  # Comparison Operator Functions
  # -----------------------------

  @doc false
  def runtime_less_than(_mfa, a, b), do: a < b
  @doc false
  def runtime_less_than_or_equal_to(_mfa, a, b), do: a <= b
  @doc false
  def runtime_equal_to(_mfa, a, b), do: a == b
  @doc false
  def runtime_not_equal_to(_mfa, a, b), do: a != b
  @doc false
  def runtime_greater_than_or_equal_to(_mfa, a, b), do: a >= b
  @doc false
  def runtime_greater_than(_mfa, a, b), do: a > b
  @doc false
  def runtime_always_true(_mfa, _a, _b), do: true
  @doc false
  def runtime_always_false(_mfa, _a, _b), do: false

  @doc false
  def abstract_less_than({:op, line, _op, a, b}), do: {:op, line, :<, a, b}
  @doc false
  def abstract_less_than_or_equal_to({:op, line, _op, a, b}), do: {:op, line, :<=, a, b}
  @doc false
  def abstract_equal_to({:op, line, _op, a, b}), do: {:op, line, :==, a, b}
  @doc false
  def abstract_not_equal_to({:op, line, _op, a, b}), do: {:op, line, :!=, a, b}
  @doc false
  def abstract_greater_than_or_equal_to({:op, line, _op, a, b}), do: {:op, line, :>=, a, b}
  @doc false
  def abstract_greater_than({:op, line, _op, a, b}), do: {:op, line, :>, a, b}
  @doc false
  # TODO: fix this
  def abstract_code_always_true({:op, _line, _op, _a, _b}), do: nil
  @doc false
  # TODO: fix this
  def abstract_code_always_false({:op, _line, _op, _a, _b}), do: nil

  @comparison_operators [:<, :<=, :==, :!=, :>=, :>]

  defguard is_comparison_operator(op) when op in @comparison_operators

  @less_than_fs_pair {:runtime_less_than, :abstract_less_than}
  @less_than_or_equal_to_fs_pair {:runtime_less_than_or_equal_to,
                                  :abstract_code_less_than_or_equal_to}
  @equal_to_fs_pair {:runtime_equal_to, :abstract_code_equal_to}
  @not_equal_to_fs_pair {:runtime_not_equal_to, :abstract_code_not_equal_to}
  @greater_than_or_equal_to_fs_pair {:runtime_greater_than_or_equal_to,
                                     :abstract_code_greater_than_or_equal_to}
  @greater_than_fs_pair {:runtime_greater_than, :abstract_code_greater_than}

  # -----------------------------
  # Arithmetic Operator Functions
  # -----------------------------

  @doc false
  def runtime_add(_mfa, a, b), do: a + b
  @doc false
  def runtime_sub(_mfa, a, b), do: a - b
  @doc false
  def runtime_mul(_mfa, a, b), do: a * b
  @doc false
  def runtime_div(_mfa, a, b), do: a / b

  @doc false
  def abstract_code_add({:op, line, _op, a, b}), do: {:op, line, :+, a, b}
  @doc false
  def abstract_code_sub({:op, line, _op, a, b}), do: {:op, line, :-, a, b}
  @doc false
  def abstract_code_mul({:op, line, _op, a, b}), do: {:op, line, :*, a, b}
  @doc false
  def abstract_code_div({:op, line, _op, a, b}), do: {:op, line, :/, a, b}

  @arithmetic_operators [:+, :-, :*, :/]

  defguard is_arithmetic_operator(op) when op in @arithmetic_operators

  @add_fs_pair {:runtime_add, :abstract_code_add}
  @sub_fs_pair {:runtime_sub, :abstract_code_sub}
  @mul_fs_pair {:runtime_mul, :abstract_code_mul}
  @div_fs_pair {:runtime_div, :abstract_code_div}

  # Mutations for operators are the following:
  # - switch order of arguments
  # - delete arguments (only for arithmetic operators)
  # - replace by arithmetic operator (only for arithmetic operators)
  # - replace by comparison operator (only for comparison operators)

  def maybe_swap_arguments(op, runtime_mfa, _abstract_code_mfa)
      when not is_commutative(op) do
    [
      [
        runtime_mfa: {__MODULE__, :runtime_swap_args, [runtime_mfa]},
        abstract_code_mfa: {__MODULE__, :abstract_code_swap_args, []},
        description: "'#{op}' operator: swap arguments",
        name: "swap arguments",
        example_input: "a #{op} b",
        example_output: "b #{op} a"
      ]
    ]
  end

  def maybe_swap_arguments(_op, _runtime_mfa, _abstract_code_mfa), do: []

  def maybe_delete_arguments(op, _runtime_mfa, _abstract_code_mfa)
      when is_arithmetic_operator(op) do
    [
      [
        runtime_mfa: {__MODULE__, :runtime_delete_left, [nil]},
        abstract_code_mfa: {__MODULE__, :abstract_code_delete_left, []},
        description: "'#{op}' operator: delete left argument",
        name: "delete left argument",
        example_input: "a #{op} b",
        example_output: "a"
      ],
      [
        runtime_mfa: {__MODULE__, :runtime_delete_right, [nil]},
        abstract_code_mfa: {__MODULE__, :abstract_code_delete_right, []},
        description: "'#{op}' operator: delete right argument",
        name: "delete right argument",
        example_input: "a #{op} b",
        example_output: "b"
      ]
    ]
  end

  def maybe_delete_arguments(_op, _runtime_mfa, _abstract_code_mfa), do: []

  def maybe_replace_by_arithmetic_operator(op, _runtime_mfa, _abstract_code_mfa)
      when is_arithmetic_operator(op) do
    ops_and_fs = [
      {:+, @add_fs_pair},
      {:-, @sub_fs_pair},
      {:*, @mul_fs_pair},
      {:/, @div_fs_pair}
    ]

    for {other_op, other_fs} <- ops_and_fs, other_op != op do
      {other_runtime_f, other_abstract_code_f} = other_fs

      [
        runtime_mfa: {__MODULE__, other_runtime_f, [nil]},
        abstract_code_mfa: {__MODULE__, other_abstract_code_f, []},
        description: "replace by '#{other_op}'",
        example_input: "a #{op} b",
        example_output: "b #{other_op} a"
      ]
    end
  end

  def maybe_replace_by_arithmetic_operator(_op, _runtime_mfa, _abstract_code_mfa), do: []

  def maybe_replace_by_comparison_operator(op, _runtime_mfa, _abstract_code_mfa)
      when is_comparison_operator(op) do
    ops_and_fs = [
      {:<, @less_than_fs_pair},
      {:<=, @less_than_or_equal_to_fs_pair},
      {:==, @equal_to_fs_pair},
      {:!=, @not_equal_to_fs_pair},
      {:>=, @greater_than_or_equal_to_fs_pair},
      {:>, @greater_than_fs_pair}
    ]

    # TODO: fix this
    for {other_op, other_fs} <- ops_and_fs, other_op != op do
      {other_runtime_f, other_abstract_code_f} = other_fs

      [
        # Argument is `nil` because it's not used
        runtime_mfa: {__MODULE__, other_runtime_f, [nil]},
        abstract_code_mfa: {__MODULE__, other_abstract_code_f, []},
        description: "replace by '#{other_op}'",
        example_input: "a #{op} b",
        example_output: "b #{other_op} a"
      ]
    end
  end

  def maybe_replace_by_comparison_operator(_op, _runtime_mfa, _abstract_code_mfa), do: []

  def maybe_replace_by_true_or_false(op, _runtime_mfa, _abstract_code_mfa)
      when op in @comparison_operators do
    [
      [
        # Argument is `nil` because it's not used
        runtime_mfa: {__MODULE__, :runtime_always_true, [nil]},
        abstract_code_mfa: {__MODULE__, :abstract_code_always_true, []},
        description: "replace by 'true'",
        example_input: "a #{op} b",
        example_output: "true"
      ],
      [
        # Argument is `nil` because it's not used
        runtime_mfa: {__MODULE__, :runtime_always_false, [nil]},
        abstract_code_mfa: {__MODULE__, :abstract_code_always_false, []},
        description: "replace by 'false'",
        example_input: "a #{op} b",
        example_output: "false"
      ]
    ]
  end

  def maybe_replace_by_true_or_false(_op, _runtime_mfa, _abstract_code_mfa), do: []

  def mutations_for_binary_operator(op, runtime_mfa, abstract_code_mfa) do
    switch_order = maybe_swap_arguments(op, runtime_mfa, abstract_code_mfa)

    delete_arguments = maybe_delete_arguments(op, runtime_mfa, abstract_code_mfa)

    replace_by_arithmetic_operator =
      maybe_replace_by_arithmetic_operator(op, runtime_mfa, abstract_code_mfa)

    replace_by_comparison_operator =
      maybe_replace_by_comparison_operator(op, runtime_mfa, abstract_code_mfa)

    replace_by_true_or_false = maybe_replace_by_true_or_false(op, runtime_mfa, abstract_code_mfa)

    Enum.concat([
      switch_order,
      delete_arguments,
      replace_by_arithmetic_operator,
      replace_by_true_or_false,
      replace_by_comparison_operator
    ])
  end
end

alias Darwin.Mutators.Default.BinOpMutatorCreator

arithmetic_operator_mutators = [
  [
    suffix: OpAddMutator,
    operator: :+,
    runtime_mfa: {BinOpMutatorCreator, :runtime_add, []},
    abstract_code_mfa: {BinOpMutatorCreator, :abstract_code_add, []}
  ],
  [
    suffix: OpSubMutator,
    operator: :-,
    runtime_mfa: {BinOpMutatorCreator, :runtime_sub, []},
    abstract_code_mfa: {BinOpMutatorCreator, :abstract_code_sub, []}
  ],
  [
    suffix: OpMulMutator,
    operator: :*,
    runtime_mfa: {BinOpMutatorCreator, :runtime_mul, []},
    abstract_code_mfa: {BinOpMutatorCreator, :abstract_code_mul, []}
  ],
  [
    suffix: OpDivMutator,
    operator: :/,
    runtime_mfa: {BinOpMutatorCreator, :runtime_div, []},
    abstract_code_mfa: {BinOpMutatorCreator, :abstract_code_div, []}
  ]
]

comparison_operator_mutators = [
  [
    suffix: OpLessThanMutator,
    operator: :<,
    runtime_mfa: {BinOpMutatorCreator, :runtime_less_than, []},
    abstract_code_mfa: {BinOpMutatorCreator, :abstract_less_than, []}
  ],
  [
    suffix: OpLessThanOrEqualToMutator,
    operator: :<=,
    runtime_mfa: {BinOpMutatorCreator, :runtime_less_than_or_equal_to, []},
    abstract_code_mfa: {BinOpMutatorCreator, :abstract_code_less_than_or_equal_to, []}
  ],
  [
    suffix: OpEqualToMutator,
    operator: :==,
    runtime_mfa: {BinOpMutatorCreator, :runtime_equal_to, []},
    abstract_code_mfa: {BinOpMutatorCreator, :abstract_equal_to, []}
  ],
  [
    suffix: OpNotEqualToMutator,
    operator: :!=,
    runtime_mfa: {BinOpMutatorCreator, :runtime_not_equal_to, []},
    abstract_code_mfa: {BinOpMutatorCreator, :abstract_not_equal_to, []}
  ],
  [
    suffix: OpGreaterThanMutator,
    operator: :>,
    runtime_mfa: {BinOpMutatorCreator, :runtime_greater_than, []},
    abstract_code_mfa: {BinOpMutatorCreator, :abstract_code_greater_than, []}
  ],
  [
    suffix: OpGreaterThanOrEqualToMutator,
    operator: :>=,
    runtime_mfa: {BinOpMutatorCreator, :runtime_greater_than_or_equal_to, []},
    abstract_code_mfa: {BinOpMutatorCreator, :abstract_code_greater_than_or_equal_to, []}
  ]
]

mutators = arithmetic_operator_mutators ++ comparison_operator_mutators

for mutator <- mutators do
  BinOpMutatorCreator.create_mutator(Darwin.Mutators.Default, mutator)
end
