defmodule Darwin.Beam do
  @moduledoc false

  @doc """
  Compile and load an erlang module.
  """
  def compile_and_load(form_list) do
    {module_name, contents} =
      case :compile.forms(form_list) do
        {:ok, module_name, contents} ->
          {module_name, contents}

        {:ok, module_name, contents, _warnings} ->
          {module_name, contents}
      end

    filename = :code.which(module_name)
    :code.load_binary(module_name, filename, contents)

    {:ok, module_name}
  end

  @doc """
  Compiles the given erlang module and runs the function.

  After running the function, the module is purged,
  even if the function raises an exception.
  """
  def with_compiled_module(form_list, fun) do
    {:ok, module_name} = compile_and_load(form_list)

    try do
      fun.()
    after
      # Delete the module to avoid the annoying warning about redefining modules.
      :code.purge(module_name)
      true = :code.delete(module_name)
    end
  end

  @doc """
  gets the erlang abstract code from the BEAM file
  """
  def beam_to_abstract_code(module) do
    filename = :code.which(module)

    {:ok, {_, [{:abstract_code, {_, abstract_code}}]}} =
      :beam_lib.chunks(filename, [:abstract_code])

    abstract_code
  end
end
