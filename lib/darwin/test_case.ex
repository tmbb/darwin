defmodule Darwin.TestCase do
  @moduledoc """
  Mostly compatible with `ExUnit.Case`.
  """

  @doc false
  defmacro __using__(_opts) do
    quote do
      # Make sure that the `test/2` and `test/3` macros are not imported!
      import ExUnit.Case, only: [describe: 2, test: 1]
      import Darwin.TestCase, only: [test: 2, test: 3]
    end
  end

  @doc """
  Defines a test with a string.

  It's the same as `ExUnit.test/3` except for some optimizations
  to avoid actually running new tests after at least one test has failed.

  ## Examples

      test "true is equal to true" do
        assert true == true
      end
  """
  defmacro test(message, var \\ quote(do: _), contents) do
    contents =
      case contents do
        [do: block] ->
          quote do
            if Darwin.MutationServer.should_this_test_run?() do
              unquote(block)
              :ok
            else
              :ok
            end
          end

        _ ->
          quote do
            if Darwin.MutationServer.should_this_test_run?() do
              try(unquote(contents))
              :ok
            else
              :ok
            end
          end
      end

    var = Macro.escape(var)
    contents = Macro.escape(contents, unquote: true)

    quote bind_quoted: [var: var, contents: contents, message: message] do
      name = ExUnit.Case.register_test(__ENV__, :test, message, [])
      def unquote(name)(unquote(var)), do: unquote(contents)
    end
  end
end
