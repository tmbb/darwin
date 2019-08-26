defmodule Darwin.Mutators.Default do
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

  def mutators() do
    [
      # Arithmetic operators
      OpAddMutator,
      OpSubMutator,
      OpMulMutator,
      OpDivMutator,

      # Strict Logical operators
      OpStrictOrMutator,
      OpStrictAndMutator,
      OpStrictNotMutator,

      # Lax Logical Operators

      # Literal Mutators
      StringMutator,
      CharlistMutator,
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

      # Mutators that ignore stuff
      IgnoreInfoMutator,
      IgnoreDefacroMutator,

      # Backup mutator - which will match everything else
      BackupMutator
    ]
  end
end
