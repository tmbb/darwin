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
