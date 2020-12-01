defmodule Darwin.DefaultMutators.RuntimeTests.FullModuleTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  import Darwin.TestHelpers

  alias Darwin.TestGenerators, as: Gen
  alias Darwin.ActiveMutation
  alias Darwin.Beam

  describe "module - single function ('+' operator):" do
    property "wrong module or codon" do
      check all(
              right_module <- Gen.module(prefix: __MODULE__),
              right_codon <- Gen.codon(),
              maybe_wrong_module <- Gen.module(prefix: __MODULE__),
              maybe_wrong_codon <- Gen.codon(),
              mutation <- Gen.mutation(),
              {maybe_wrong_module, maybe_wrong_codon} != {right_module, right_codon},
              a <- one_of([float(), integer()]),
              b <- one_of([float(), integer()])
            ) do
        # Mutate a module containing a simple elixir function
        {mutated_forms_list, _ctx} =
          mutate_elixir_module(right_module, """
            def f(x, y) do
              x + y
            end
          """)

        # Compile the module and run the tests
        Beam.with_compiled_module(mutated_forms_list, fn ->
          ActiveMutation.with_mutation({maybe_wrong_module, maybe_wrong_codon, mutation}, fn ->
            assert right_module.f(a, b) == a + b
          end)
        end)
      end
    end

    property "right codon, wrong mutations" do
      check all(
              right_module <- Gen.module(prefix: __MODULE__),
              right_codon <- Gen.codon(),
              wrong_mutation <- Gen.mutation(min: 3),
              a <- one_of([float(), integer()]),
              b <- one_of([float(), integer()])
            ) do
        # Mutate a module containing a simple elixir function
        {mutated_forms_list, _ctx} =
          mutate_elixir_module(right_module, """
            def f(x, y) do
              x + y
            end
          """)

        # Compile the module and run the tests
        Beam.with_compiled_module(mutated_forms_list, fn ->
          ActiveMutation.with_mutation({right_module, right_codon, wrong_mutation}, fn ->
            assert right_module.f(a, b) == a + b
          end)
        end)
      end
    end

    property "Mutation 0 - replace by '-'" do
      check all(
              module <- Gen.module(prefix: __MODULE__),
              codon = 0,
              mutation = 0,
              a <- one_of([float(), integer()]),
              b <- one_of([float(), integer()])
            ) do
        # Mutate a module containing a simple elixir function
        {mutated_forms_list, _ctx} =
          mutate_elixir_module(module, """
            def f(x, y) do
              x + y
            end
          """)

        # Compile the module and run the tests
        Beam.with_compiled_module(mutated_forms_list, fn ->
          ActiveMutation.with_mutation({module, codon, mutation}, fn ->
            assert module.f(a, b) == a - b
          end)
        end)
      end
    end

    property "Mutation 1 - replace by '*'" do
      check all(
              module <- Gen.module(prefix: __MODULE__),
              codon = 0,
              mutation = 1,
              a <- one_of([float(), integer()]),
              b <- one_of([float(), integer()])
            ) do
        # Mutate a module containing a simple elixir function
        {mutated_forms_list, _ctx} =
          mutate_elixir_module(module, """
            def f(x, y) do
              x + y
            end
          """)

        # Compile the module and run the tests
        Beam.with_compiled_module(mutated_forms_list, fn ->
          ActiveMutation.with_mutation({module, codon, mutation}, fn ->
            assert module.f(a, b) == a * b
          end)
        end)
      end
    end

    property "Mutation 2 - replace by '/'" do
      check all(
              module <- Gen.module(prefix: __MODULE__),
              codon = 0,
              mutation = 2,
              a <- one_of([float(), integer()]),
              b <- one_of([float(), integer()]),
              b != 0
            ) do
        # Mutate a module containing a simple elixir function
        {mutated_forms_list, _ctx} =
          mutate_elixir_module(module, """
            def f(x, y) do
              x + y
            end
          """)

        # Compile the module and run the tests
        Beam.with_compiled_module(mutated_forms_list, fn ->
          ActiveMutation.with_mutation({module, codon, mutation}, fn ->
            assert module.f(a, b) == a / b
          end)
        end)
      end
    end
  end

  test "module - two functions" do
    # Compile the module and run the tests
    check all(
            module <- Gen.module(prefix: __MODULE__),
            a <- one_of([float(), integer()]),
            b <- one_of([float(), integer()]),
            c <- one_of([float(), integer()]),
            wrong_mutation <- Gen.mutation(min: 4)
          ) do
      # Mutate a module containing a simple elixir function
      {mutated_forms_list, _ctx} =
        mutate_elixir_module(module, """
          def f(x, y, z) do
            {[x * y - z], x / z}
          end
        """)

      # Run the compiled code
      Beam.with_compiled_module(mutated_forms_list, fn ->
        # Codon 0 - mutates the '*' operator
        ActiveMutation.with_mutation({module, 0, 0}, fn ->
          if c != 0 do
            assert module.f(a, b, c) == {[a + b - c], a / c}
          end
        end)

        ActiveMutation.with_mutation({module, 0, 1}, fn ->
          if c != 0 do
            assert module.f(a, b, c) == {[a - b - c], a / c}
          end
        end)

        ActiveMutation.with_mutation({module, 0, 2}, fn ->
          if b != 0 and c != 0 do
            assert module.f(a, b, c) == {[a / b - c], a / c}
          end
        end)

        # Codon 1 - mutates the '-' operator
        ActiveMutation.with_mutation({module, 1, 0}, fn ->
          if c != 0 do
            assert module.f(a, b, c) == {[a * b + c], a / c}
          end
        end)

        ActiveMutation.with_mutation({module, 1, 1}, fn ->
          if c != 0 do
            assert module.f(a, b, c) == {[a * b * c], a / c}
          end
        end)

        ActiveMutation.with_mutation({module, 1, 2}, fn ->
          if c != 0 do
            assert module.f(a, b, c) == {[a * b / c], a / c}
          end
        end)

        ActiveMutation.with_mutation({module, 1, 3}, fn ->
          if c != 0 do
            assert module.f(a, b, c) == {[c - a * b], a / c}
          end
        end)

        # Codon 2 - mutates the '/' operator
        ActiveMutation.with_mutation({module, 2, 0}, fn ->
          assert module.f(a, b, c) == {[a * b - c], a + c}
        end)

        ActiveMutation.with_mutation({module, 2, 1}, fn ->
          assert module.f(a, b, c) == {[a * b - c], a - c}
        end)

        ActiveMutation.with_mutation({module, 2, 2}, fn ->
          assert module.f(a, b, c) == {[a * b - c], a * c}
        end)

        ActiveMutation.with_mutation({module, 2, 3}, fn ->
          if a != 0 do
            assert module.f(a, b, c) == {[a * b - c], c / a}
          end
        end)

        # Test the three codons with the wrong mutation:
        ActiveMutation.with_mutation({module, 0, wrong_mutation}, fn ->
          if c != 0 do
            assert module.f(a, b, c) == {[a * b - c], a / c}
          end
        end)

        ActiveMutation.with_mutation({module, 1, wrong_mutation}, fn ->
          if c != 0 do
            assert module.f(a, b, c) == {[a * b - c], a / c}
          end
        end)

        ActiveMutation.with_mutation({module, 2, wrong_mutation}, fn ->
          if c != 0 do
            assert module.f(a, b, c) == {[a * b - c], a / c}
          end
        end)
      end)
    end
  end
end
