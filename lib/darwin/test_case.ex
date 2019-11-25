defmodule Darwin.TestCase do
  @doc false
  defmacro __using__(opts) do
    unless Process.whereis(ExUnit.Server) do
      raise "cannot use ExUnit.Case without starting the ExUnit application, " <>
              "please call ExUnit.start() or explicitly start the :ex_unit app"
    end

    quote do
      async = !!unquote(opts)[:async]

      unless Module.get_attribute(__MODULE__, :ex_unit_tests) do
        moduletag_check = Module.get_attribute(__MODULE__, :moduletag)
        tag_check = Module.get_attribute(__MODULE__, :tag)

        if moduletag_check || tag_check do
          raise "you must set @tag and @moduletag after the call to \"use ExUnit.Case\""
        end

        attributes = [
          :ex_unit_tests,
          :tag,
          :describetag,
          :moduletag,
          :ex_unit_registered,
          :ex_unit_used_describes
        ]

        Enum.each(attributes, &Module.register_attribute(__MODULE__, &1, accumulate: true))

        @before_compile ExUnit.Case
        @after_compile ExUnit.Case
        @ex_unit_async async
        @ex_unit_describe nil
        use ExUnit.Callbacks
      end

      import ExUnit.Callbacks
      import ExUnit.Assertions
      import ExUnit.Case, only: [describe: 2, test: 1]
      import Darwin.TestCase, only: [test: 2, test: 3]
      import ExUnit.DocTest
    end
  end

  @doc """
  Defines a test with a string.
  Provides a convenient macro that allows a test to be
  defined with a string. This macro automatically inserts
  the atom `:ok` as the last line of the test. That said,
  a passing test always returns `:ok`, but, more importantly,
  it forces Elixir to not tail call optimize the test and
  therefore avoids hiding lines from the backtrace.
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
