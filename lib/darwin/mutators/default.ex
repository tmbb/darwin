defmodule Darwin.Mutators.Default do
  alias Darwin.Mutators.Default.{
    # Arithmetic operators
    # OpAddMutator,
    # OpSubMutator,
    # OpMulMutator,
    # OpDivMutator,
    # Logic operators
    OpStrictOrMutator,
    OpStrictAndMutator,
    OpStrictNotMutator,
    # Literal Mutators
    CharlistMutator,
    StringMutator,
    AtomMutator
    # Bitshift operators
    # Comparison operators
    # OpLessThanMutator,
    # OpLessThanOrEqualToMutator,
    # OpEqualToMutator,
    # OpNotEqualToMutator,
    # OpGreaterThanMutator,
    # OpGreaterThanOrEqualToMutator,
    # OpGreaterThanMutator
  }

  def mutators() do
    [
      # Arithmetic operators
      # OpAddMutator,
      # OpSubMutator,
      # OpMulMutator,
      # OpDivMutator,

      # Logic operators
      OpStrictOrMutator,
      OpStrictAndMutator,
      OpStrictNotMutator,

      # Literal Mutators
      StringMutator,
      CharlistMutator,
      AtomMutator,

      # Bitshift operators

      # Comparison operators
      # OpLessThanMutator,
      # OpLessThanOrEqualToMutator,
      # OpEqualToMutator,
      # OpNotEqualToMutator,
      # OpGreaterThanMutator,
      # OpGreaterThanOrEqualToMutator,
      # OpGreaterThanMutator

      # Backup mutator - which will match everything else
      Darwin.Mutators.Common.BackupMutator
    ]
  end
end
