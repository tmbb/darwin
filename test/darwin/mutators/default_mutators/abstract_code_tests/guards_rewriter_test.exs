defmodule Darwin.DefaultMutators.AbstractCodeTests.GuardsRewriterTest do
  use ExUnit.Case
  import Darwin.TestHelpers
  alias Darwin.Erlang

  @outputs "test/darwin/mutators/default_mutators/abstract_code_tests/outputs/guards_rewriter/"

  # We can't use `Erlang.assert_equivalent` when comparing entire modules.
  # I'm not really sure why it fails (erlang somehow doesn't parse the file correctly).
  # In the meantime, we'll use literal string comparison, even if we know it's much more brittle...

  @tag :skip
  test "example 1" do
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

    output_path = Path.join(@outputs, "example_1/mutated.erl")
    m_mutated = __MODULE__.Example1_Module_Mutated
    expected = File.read!(output_path)

    {mutated_abstract_code, _ctx} = mutate_and_compile_elixir(elixir_function, module: m_mutated)
    erlang_source = Erlang.pprint_forms(mutated_abstract_code)

    assert erlang_source == expected
  end

  @tag :skip
  test "example 2: code from the `Enum` module" do
    elixir_function =
      quote do
        def f(a, b), do: random_integer(a, b)

        defp random_integer(limit, limit) when is_integer(limit) do
          limit
        end

        defp random_integer(lower_limit, upper_limit) when upper_limit < lower_limit do
          random_integer(upper_limit, lower_limit)
        end

        defp random_integer(lower_limit, upper_limit) do
          lower_limit + :rand.uniform(upper_limit - lower_limit + 1) - 1
        end
      end

    output_path = Path.join(@outputs, "example_2/mutated.erl")
    m_mutated = __MODULE__.Example2_Module_Mutated
    expected = File.read!(output_path)

    {mutated_abstract_code, _ctx} = mutate_and_compile_elixir(elixir_function, module: m_mutated)
    erlang_source = Erlang.pprint_forms(mutated_abstract_code)

    assert erlang_source == expected
  end
end
