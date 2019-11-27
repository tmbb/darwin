defmodule Darwin.DefaultMutators.RuntimeTests.ArithmeticOperatorsTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  alias DarwinTest.Generators, as: Gen
  alias Darwin.ActiveMutation

  alias Darwin.Mutators.Default.{
    OpAddMutator,
    OpSubMutator,
    OpMulMutator,
    OpDivMutator
  }

  describe "operator: '+' :" do
    property "wrong module or codon" do
      check all right_module <- Gen.module(),
                right_codon <- Gen.codon(),
                maybe_wrong_module <- Gen.module(),
                maybe_wrong_codon <- Gen.codon(),
                mutation <- Gen.mutation(),
                {maybe_wrong_module, maybe_wrong_codon} != {right_module, right_codon},
                a <- one_of([float(), integer()]),
                b <- one_of([float(), integer()]) do
        ActiveMutation.with_mutation({right_module, right_codon, mutation}, fn ->
          assert OpAddMutator.darwin_was_here(maybe_wrong_module, maybe_wrong_codon, a, b) ==
                   a + b
        end)
      end
    end

    property "right codon, wrong mutations" do
      check all module <- Gen.module(),
                codon <- Gen.codon(),
                mutation <- Gen.mutation(),
                mutation >= 3,
                a <- one_of([float(), integer()]),
                b <- one_of([float(), integer()]) do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpAddMutator.darwin_was_here(module, codon, a, b) == a + b
        end)
      end
    end

    property "Mutation 0 - replace by '-'" do
      check all module <- Gen.module(),
                codon <- Gen.codon(),
                mutation = 0,
                a <- one_of([float(), integer()]),
                b <- one_of([float(), integer()]) do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpAddMutator.darwin_was_here(module, codon, a, b) == a - b
        end)
      end
    end

    property "Mutation 1 - replace by '*'" do
      check all module <- Gen.module(),
                codon <- Gen.codon(),
                mutation = 1,
                a <- one_of([float(), integer()]),
                b <- one_of([float(), integer()]) do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpAddMutator.darwin_was_here(module, codon, a, b) == a * b
        end)
      end
    end

    property "Mutation 2 - replace by '/'" do
      check all module <- Gen.module(),
                codon <- Gen.codon(),
                mutation = 2,
                a <- one_of([float(), integer()]),
                b <- one_of([float(), integer()]),
                b != 0 do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpAddMutator.darwin_was_here(module, codon, a, b) == a / b
        end)
      end
    end
  end

  describe "operator: '-' :" do
    property "wrong module or codon" do
      check all right_module <- Gen.module(),
                right_codon <- Gen.codon(),
                maybe_wrong_module <- Gen.module(),
                maybe_wrong_codon <- Gen.codon(),
                mutation <- Gen.mutation(),
                {maybe_wrong_module, maybe_wrong_codon} != {right_module, right_codon},
                a <- one_of([float(), integer()]),
                b <- one_of([float(), integer()]) do
        ActiveMutation.with_mutation({right_module, right_codon, mutation}, fn ->
          assert OpSubMutator.darwin_was_here(maybe_wrong_module, maybe_wrong_codon, a, b) ==
                   a - b
        end)
      end
    end

    property "right codon, wrong mutations" do
      check all module <- Gen.module(),
                codon <- Gen.codon(),
                mutation <- Gen.mutation(),
                mutation >= 4,
                a <- one_of([float(), integer()]),
                b <- one_of([float(), integer()]) do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpSubMutator.darwin_was_here(module, codon, a, b) == a - b
        end)
      end
    end

    property "Mutation 0 - replace by '+'" do
      check all module <- Gen.module(),
                codon <- Gen.codon(),
                mutation = 0,
                a <- one_of([float(), integer()]),
                b <- one_of([float(), integer()]) do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpSubMutator.darwin_was_here(module, codon, a, b) == a + b
        end)
      end
    end

    property "Mutation 1 - replace by '*'" do
      check all module <- Gen.module(),
                codon <- Gen.codon(),
                mutation = 1,
                a <- one_of([float(), integer()]),
                b <- one_of([float(), integer()]) do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpSubMutator.darwin_was_here(module, codon, a, b) == a * b
        end)
      end
    end

    property "Mutation 2 - replace by '/'" do
      check all module <- Gen.module(),
                codon <- Gen.codon(),
                mutation = 2,
                a <- one_of([float(), integer()]),
                b <- one_of([float(), integer()]),
                b != 0 do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpSubMutator.darwin_was_here(module, codon, a, b) == a / b
        end)
      end
    end

    property "Mutation 3 - swap arguments" do
      check all module <- Gen.module(),
                codon <- Gen.codon(),
                mutation = 3,
                a <- one_of([float(), integer()]),
                b <- one_of([float(), integer()]) do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpSubMutator.darwin_was_here(module, codon, a, b) == b - a
        end)
      end
    end
  end

  describe "operator: '*' :" do
    property "wrong module or codon" do
      check all right_module <- Gen.module(),
                right_codon <- Gen.codon(),
                maybe_wrong_module <- Gen.module(),
                maybe_wrong_codon <- Gen.codon(),
                mutation <- Gen.mutation(),
                {maybe_wrong_module, maybe_wrong_codon} != {right_module, right_codon},
                a <- one_of([float(), integer()]),
                b <- one_of([float(), integer()]) do
        ActiveMutation.with_mutation({right_module, right_codon, mutation}, fn ->
          assert OpMulMutator.darwin_was_here(maybe_wrong_module, maybe_wrong_codon, a, b) ==
                   a * b
        end)
      end
    end

    property "right codon, wrong mutations" do
      check all module <- Gen.module(),
                codon <- Gen.codon(),
                mutation <- Gen.mutation(),
                mutation > 3,
                a <- one_of([float(), integer()]),
                b <- one_of([float(), integer()]) do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpMulMutator.darwin_was_here(module, codon, a, b) == a * b
        end)
      end
    end

    property "Mutation 0 - replace by '+'" do
      check all module <- Gen.module(),
                codon <- Gen.codon(),
                mutation = 0,
                a <- one_of([float(), integer()]),
                b <- one_of([float(), integer()]) do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpMulMutator.darwin_was_here(module, codon, a, b) == a + b
        end)
      end
    end

    property "Mutation 1 - replace by '-'" do
      check all module <- Gen.module(),
                codon <- Gen.codon(),
                mutation = 1,
                a <- one_of([float(), integer()]),
                b <- one_of([float(), integer()]) do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpMulMutator.darwin_was_here(module, codon, a, b) == a - b
        end)
      end
    end

    property "Mutation 2 - replace by '/'" do
      check all module <- Gen.module(),
                codon <- Gen.codon(),
                mutation = 2,
                a <- one_of([float(), integer()]),
                b <- one_of([float(), integer()]),
                b != 0 do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpMulMutator.darwin_was_here(module, codon, a, b) == a / b
        end)
      end
    end
  end

  describe "operator: '/' :" do
    property "wrong module or codon" do
      check all right_module <- Gen.module(),
                right_codon <- Gen.codon(),
                maybe_wrong_module <- Gen.module(),
                maybe_wrong_codon <- Gen.codon(),
                mutation <- Gen.mutation(),
                {maybe_wrong_module, maybe_wrong_codon} != {right_module, right_codon},
                a <- one_of([float(), integer()]),
                b <- one_of([float(), integer()]),
                b != 0 do
        ActiveMutation.with_mutation({right_module, right_codon, mutation}, fn ->
          assert OpDivMutator.darwin_was_here(maybe_wrong_module, maybe_wrong_codon, a, b) ==
                   a / b
        end)
      end
    end

    property "right codon, wrong mutations" do
      check all module <- Gen.module(),
                codon <- Gen.codon(),
                mutation <- Gen.mutation(),
                mutation > 3,
                a <- one_of([float(), integer()]),
                b <- one_of([float(), integer()]),
                b != 0 do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpDivMutator.darwin_was_here(module, codon, a, b) == a / b
        end)
      end
    end

    property "Mutation 0 - replace by '+'" do
      check all module <- Gen.module(),
                codon <- Gen.codon(),
                mutation = 0,
                a <- one_of([float(), integer()]),
                b <- one_of([float(), integer()]) do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpDivMutator.darwin_was_here(module, codon, a, b) == a + b
        end)
      end
    end

    property "Mutation 1 - replace by '-'" do
      check all module <- Gen.module(),
                codon <- Gen.codon(),
                mutation = 1,
                a <- one_of([float(), integer()]),
                b <- one_of([float(), integer()]) do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpDivMutator.darwin_was_here(module, codon, a, b) == a - b
        end)
      end
    end

    property "Mutation 2 - replace by '*'" do
      check all module <- Gen.module(),
                codon <- Gen.codon(),
                mutation = 2,
                a <- one_of([float(), integer()]),
                b <- one_of([float(), integer()]) do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpDivMutator.darwin_was_here(module, codon, a, b) == a * b
        end)
      end
    end

    property "Mutation 3 - swap arguments" do
      check all module <- Gen.module(),
                codon <- Gen.codon(),
                mutation = 3,
                a <- one_of([float(), integer()]),
                b <- one_of([float(), integer()]) do
        denom = if a == 0, do: 0.1, else: a

        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpDivMutator.darwin_was_here(module, codon, denom, b) == b / denom
        end)
      end
    end
  end
end
