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

    # Stricts Logical Operators
    OpStrictOrMutator,
    OpStrictAndMutator,
    OpStrictNotMutator,

    # Literal Mutators
    CharlistMutator,
    StringMutator,
    AtomMutator,

    # Bitshift operators

    # Comparison operators
    OpLessThanMutator,
    OpLessThanOrEqualToMutator,
    OpEqualToMutator,
    OpNotEqualToMutator,
    OpGreaterThanMutator,
    OpGreaterThanOrEqualToMutator,
    OpGreaterThanMutator,

    # Bitshift operators

    # Things to ignore
    IgnoreInfoMutator,
    IgnoreDefacroMutator
  }

  alias Darwin.Mutators.Common.BackupMutator

  @doc """
  Arithmetic operator mutators.
  """
  def arithmetic_operator_mutators() do
    [
      OpAddMutator,
      OpSubMutator,
      OpMulMutator,
      OpDivMutator
    ]
  end

  @doc """
  Strict logical operator mutators.
  """
  def strict_logical_operator_mutators() do
    [
      OpStrictOrMutator,
      OpStrictAndMutator,
      OpStrictNotMutator
    ]
  end

  @doc """
  Literal mutators.
  """
  def literal_mutators() do
    [
      StringMutator,
      CharlistMutator,
      AtomMutator
    ]
  end

  @doc """
  Comparison operator mutators.
  """
  def comparison_operators() do
    [
      OpLessThanMutator,
      OpLessThanOrEqualToMutator,
      OpEqualToMutator,
      OpNotEqualToMutator,
      OpGreaterThanMutator,
      OpGreaterThanOrEqualToMutator,
      OpGreaterThanMutator
    ]
  end

  @doc """
  The default list of mutators.
  """
  def mutators() do
    named_mutators =
      arithmetic_operator_mutators() ++
        strict_logical_operator_mutators() ++ literal_mutators() ++ comparison_operators()

    named_mutators ++
      [
        # Lax Logical Operators - MISSING

        # Bitshift operators - MISSING

        # Mutators that ignore stuff
        IgnoreInfoMutator,
        IgnoreDefacroMutator,

        # Backup mutator - which will match everything else
        BackupMutator
      ]
  end
end
