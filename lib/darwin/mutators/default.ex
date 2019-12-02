defmodule Darwin.Mutators.Default do
  @moduledoc """
  A module containing the default list of mutators.

  Darwing is meant to be fully modular, and you can define your own
  custom list of mutators.
  """
  alias Darwin.Mutators.Default.{
    # Arithmetic operators
    OpAddMutator,
    OpSubMutator,
    OpMulMutator,
    OpDivMutator,

    # Strict Logical Operators
    OpStrictOrMutator,
    OpStrictAndMutator,
    OpStrictNotMutator,

    # Lax Logical Operators
    OpLaxOrMutator,
    OpLaxAndMutator,
    OpLaxNotMutator,

    # Literal Mutators
    CharlistMutator,
    StringMutator,
    AtomMutator,
    IntegerMutator,

    # Bitshift operators

    # Comparison operators
    OpLessThanMutator,
    OpLessThanOrEqualToMutator,
    OpEqualToMutator,
    OpNotEqualToMutator,
    OpGreaterThanMutator,
    OpGreaterThanOrEqualToMutator,
    OpGreaterThanMutator,

    # Things to ignore
    IgnoreInfoMutator,
    IgnoreDefacroMutator
  }

  alias Darwin.Mutators.Common.{
    BackupMutator,
    GuardRewriterMutator
  }

  print_list = fn list ->
    items =
      for m <- list do
        "- `#{inspect(m)}`"
      end

    Enum.join(items, "\n")
  end

  @ignore_mutators [
    IgnoreInfoMutator,
    IgnoreDefacroMutator
  ]

  @arithmetic_operator_mutators [
    OpAddMutator,
    OpSubMutator,
    OpMulMutator,
    OpDivMutator
  ]

  @strict_logical_operator_mutators [
    OpStrictOrMutator,
    OpStrictAndMutator,
    OpStrictNotMutator
  ]

  @lax_logical_operator_mutators [
    OpLaxOrMutator,
    OpLaxAndMutator,
    OpLaxNotMutator
  ]

  @literal_mutators [
    StringMutator,
    CharlistMutator,
    AtomMutator,
    IntegerMutator
  ]

  @comparison_operators [
    OpLessThanMutator,
    OpLessThanOrEqualToMutator,
    OpEqualToMutator,
    OpNotEqualToMutator,
    OpGreaterThanMutator,
    OpGreaterThanOrEqualToMutator,
    OpGreaterThanMutator
  ]

  @miscelaneous_mutators [
    GuardRewriterMutator,
    BackupMutator
  ]

  @all_mutators @ignore_mutators ++
                  @arithmetic_operator_mutators ++
                  @strict_logical_operator_mutators ++
                  @lax_logical_operator_mutators ++
                  @literal_mutators ++ @comparison_operators ++ @miscelaneous_mutators

  @doc """
  Mutators for the arithmetic operators.

  Contains:
  #{print_list.(@arithmetic_operator_mutators)}
  """
  def arithmetic_operator_mutators() do
    @arithmetic_operator_mutators
  end

  @doc """
  Mutators for the strict logical operators (`or`, `and`, `not`).

  Contains:
  #{print_list.(@strict_logical_operator_mutators)}
  """
  def strict_logical_operator_mutators() do
    @strict_logical_operator_mutators
  end

  @doc """
  Mutators for the lax logical operators (`||`, `&&`, `!`).

  Contains:
  #{print_list.(@lax_logical_operator_mutators)}
  """
  def lax_logical_operator_mutators() do
    @lax_logical_operator_mutators
  end

  @doc """
  Mutators for literals.

  Contains:
  #{print_list.(@literal_mutators)}
  """
  def literal_mutators() do
    @literal_mutators
  end

  @doc """
  Comparison operator mutators.

  Contains:
  #{print_list.(@comparison_operators)}
  """
  def comparison_operators() do
    @comparison_operators
  end

  @doc """
  The default list of mutators.

  Contains (in this order):
  #{print_list.(@all_mutators)}

  The order in which the mutators are applied is important.
  The `#{inspect(BackupMutator)}` must always come last (because it matches everything).
  """
  def mutators() do
    @all_mutators
  end
end
