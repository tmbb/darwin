defmodule Darwin.Beam do
  @moduledoc false

  @filename to_charlist(Path.join(__DIR__, "beam.ex"))

  @doc """
  Compiles a form list into a BEAM module
  """
  def compile(form_list) do
    :compile.forms(form_list)
  end

  @doc """
  Compiles the given erlang module and runs the function.

  After running the function, the module is purged,
  even if the function raises an exception.
  """
  def with_compiled_module(form_list, fun) do
    {module_name, contents} =
      case compile(form_list) do
        {:ok, module_name, contents} ->
          {module_name, contents}

        {:ok, module_name, contents, _warnings} ->
          {module_name, contents}
      end

    :code.load_binary(module_name, @filename, contents)

    try do
      fun.()
    after
      # Delete the module to avoid the annoying warning about redefining modules.
      :code.purge(module_name)
      true = :code.delete(module_name)
    end
  end
end
