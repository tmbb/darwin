defmodule Darwin.Mutators.Default.IgnoreInfoMutator do
  @behaviour Darwin.Mutator
  @moduledoc """
  Don't mutate anything inside the `__info__/1` function elixir defines
  in every module.
  """

  @impl true
  def mutate({:function, _line, :__info__, 1 = _arity, _clauses} = abstract_code, ctx) do
    {:ok, {abstract_code, ctx}}
  end

  def mutate(_abstract_code, _ctx), do: :error
end
