defmodule Darwin.Utils.SafeSourcePath do
  @moduledoc false

  @doc """
  Extracts a module path in a safe (but hacky!) way.
  It might return `nil`
  """
  def source_path_for_module(module) do
    try do
      charlist =
        module.module_info(:compile)
        |> Keyword.get(:source)

      if charlist do
        to_string(charlist)
      else
        nil
      end
    rescue
      _ in UndefinedFunctionError ->
        nil
    end
  end
end
