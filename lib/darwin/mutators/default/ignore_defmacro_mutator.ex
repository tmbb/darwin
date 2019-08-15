defmodule Darwin.Mutators.Default.IgnoreDefacroMutator do
  @moduledoc """
  Don't mutate anything inside a macro definition.
  """

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
