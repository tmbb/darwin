defmodule Darwin.DefaultMutators.RuntimeTests.GuardsRewriterTest do
  use ExUnit.Case
  import DarwinTest.Helpers

  test "example" do
    elixir_function =
      quote do
        # Function clause with guard that never raises an error
        def f(x) when x == 1, do: :f1
        # Function clause with guard that may raise an error
        # if ´x` is not a tuple
        # (tests for the fact that errors in guards simply mean that
        # we go to the next clause)
        def f(x) when elem(x, 1) == 1, do: :f2
        # Function clause without guards
        def f({_, 2}), do: :f3
        # All other arguments should raise a `FunctionClauseError`
      end

    # Create two module names so that we can test both the mutated and unmutated versions
    m_original = __MODULE__.Example1_Module_Original
    m_mutated = __MODULE__.Example1_Module_Mutated

    compile_elixir(elixir_function, module: m_original)
    mutate_and_compile_elixir(elixir_function, module: m_mutated)

    # Test compatibility between the two
    for m <- [m_original, m_mutated] do
      # Assert result of the first clause:
      assert m.f(1) == :f1
      # Assert result of the second clause:
      assert m.f({0, 1}) == :f2
      # Assert result of the third clause:
      assert m.f({0, 2}) == :f3
      # Assert that everything that doesn't match the clauses above
      # raises a `FunctionClauseError`:
      assert_raise FunctionClauseError, fn ->
        m.f(nil)
      end
    end
  end
end
