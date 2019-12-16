defmodule Darwin.Mutators.Default.IgnoreDefacroMutator do
  @behaviour Darwin.Mutator
  @moduledoc """
  Don't mutate anything inside a macro definition.
  """

  @impl true
  def mutate({:function, _line, atom_name, _arity, _clauses} = abstract_code, ctx) do
    string_name = Atom.to_string(atom_name)

    case String.starts_with?(string_name, "MACRO-") do
      true ->
        {:ok, {abstract_code, ctx}}

      false ->
        :error
    end
  end

  def mutate(_abstract_code, _ctx), do: :error
end
