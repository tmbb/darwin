defmodule Darwin.Mutators.Common.RespectPatternRewritesMutator do
  @behaviour Darwin.Mutator
  alias Darwin.Mutator

  # This mutators is not meant to be used by normal users.
  # It«s just a helper for other mutators.

  @moduledoc false

  @impl true
  def mutate({:op, line, :==, left, right}, ctx) when line == 1_000_000_000_000 do
    # Don't mutate the equals operator itself, only its arguments
    # Only the right side will be mutated
    {mutated_left, ctx} = Mutator.do_mutate(left, ctx)
    {mutated_right, ctx} = Mutator.do_mutate(right, ctx)

    # Set the line to zero because `-1` is an illegal value
    mutated_abstract_code = {:op, 0, :==, mutated_left, mutated_right}

    {:ok, {mutated_abstract_code, ctx}}
  end

  def mutate(_abstract_code, _ctx), do: :error
end
