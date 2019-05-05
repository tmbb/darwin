defmodule Darwin.TestCase do
  defmacro __using__(opts) do
    _modules = Keyword.fetch!(opts, :modules)

    quote do
      use ExUnit.Case, async: false
    end
  end
end
