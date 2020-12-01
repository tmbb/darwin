defmodule Darwin.DefaultMutators.RuntimeTests.AnonymousFunctionTest do
  use ExUnit.Case, async: true
  import Darwin.TestHelpers
  import ExUnitProperties

  test "example" do
    elixir_function =
      quote do
        def f(g, x) do
          fun = fn
            # We'll mutate the function patterns
            {:ok, value} -> {:ok, g.(value)}
            :error -> :error
          end

          fun.(x)
        end
      end

    # Create two module names so that we can test both the mutated and unmutated versions
    m_original = __MODULE__.Example1_Module_Original
    m_mutated = __MODULE__.Example1_Module_Mutated

    compile_elixir(elixir_function, module: m_original)
    mutate_and_compile_elixir(elixir_function, module: m_mutated)

    # Test compatibility between the two versions of the module
    check all(
            x <- StreamData.integer(),
            k <- StreamData.integer(),
            fun = fn n -> k * n end
          ) do
      # Original module
      assert m_original.f(fun, {:ok, x}) == {:ok, fun.(x)}
      assert m_original.f(fun, :error) == :error
      assert_raise FunctionClauseError, fn -> m_original.f(fun, :invalid) end
      # Mutated module
      assert m_mutated.f(fun, {:ok, x}) == {:ok, fun.(x)}
      assert m_mutated.f(fun, :error) == :error
      assert_raise FunctionClauseError, fn -> m_mutated.f(fun, :invalid) end
    end
  end
end
