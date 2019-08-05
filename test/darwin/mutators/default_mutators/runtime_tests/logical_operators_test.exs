defmodule Darwin.DefaultMutators.RuntimeTests.LogicalOperatorsTest do
  use ExUnit.Case, async: true
  alias Darwin.ActiveMutation

  alias Darwin.Mutators.Default.{
    OpStrictOrMutator,
    OpStrictAndMutator,
    OpStrictNotMutator
  }

  # The right module
  # The actual value doesn't matter
  @module MyModule
  # Some other codon number
  @other_module AnotherModule
  # Available modules
  @modules [
    @module,
    @other_module
  ]
  # The right codon number
  # The actual value doesn't matter
  @codon 0
  # Some other codon number
  @other_codon 1
  # Available codons
  @codons [
    @codon,
    @other_codon
  ]

  @max_mutations 8

  doctest Darwin

  describe "operator: not -" do
    test "wrong module or codon" do
      # If the module and the codon are wrong, the function is compatible
      for module <- @modules,
          codon <- @codons,
          mutation <- 0..@max_mutations,
          {module, codon} != {@module, @codon} do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          # assert module != @module or codon != @codon
          assert OpStrictNotMutator.do_mutate(@module, @codon, true) == not true
          assert OpStrictNotMutator.do_mutate(@module, @codon, false) == not false

          assert_raise ArgumentError, fn ->
            OpStrictNotMutator.do_mutate(@module, @codon, :not_a_boolean)
          end
        end)
      end
    end

    test "right codon, wrong mutations" do
      for mutation <- 3..@max_mutations do
        ActiveMutation.with_mutation({@module, @codon, mutation}, fn ->
          # assert module != @module or codon != @codon
          assert OpStrictNotMutator.do_mutate(@module, @codon, true) == not true
          assert OpStrictNotMutator.do_mutate(@module, @codon, false) == not false

          assert_raise ArgumentError, fn ->
            OpStrictNotMutator.do_mutate(@module, @codon, :not_a_boolean)
          end
        end)
      end
    end

    test "Mutation 0 - remove negation" do
      ActiveMutation.with_mutation({@module, @codon, 0}, fn ->
        assert OpStrictNotMutator.do_mutate(@module, @codon, true) == true
        assert OpStrictNotMutator.do_mutate(@module, @codon, false) == false
        assert OpStrictNotMutator.do_mutate(@module, @codon, :other) == :other
      end)
    end

    test "Mutation 1 - replace by `true`" do
      ActiveMutation.with_mutation({@module, @codon, 1}, fn ->
        assert OpStrictNotMutator.do_mutate(@module, @codon, true) == true
        assert OpStrictNotMutator.do_mutate(@module, @codon, false) == true
        assert OpStrictNotMutator.do_mutate(@module, @codon, :other) == true
      end)
    end

    test "Mutation 2 - replace by `false`" do
      ActiveMutation.with_mutation({@module, @codon, 2}, fn ->
        assert OpStrictNotMutator.do_mutate(@module, @codon, true) == false
        assert OpStrictNotMutator.do_mutate(@module, @codon, false) == false
        assert OpStrictNotMutator.do_mutate(@module, @codon, :other) == false
      end)
    end
  end

  describe "operator: and -" do
    test "wrong module or codon" do
      # If the module and the codon are wrong, the function is compatible
      for module <- @modules,
          codon <- @codons,
          mutation <- 0..@max_mutations,
          {module, codon} != {@module, @codon} do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpStrictAndMutator.do_mutate(@module, @codon, true, true) == (true and true)
          assert OpStrictAndMutator.do_mutate(@module, @codon, true, false) == (true and false)
          assert OpStrictAndMutator.do_mutate(@module, @codon, false, true) == (false and true)
          assert OpStrictAndMutator.do_mutate(@module, @codon, false, false) == (false and false)

          assert_raise BadBooleanError, fn ->
            OpStrictAndMutator.do_mutate(@module, @codon, :not_a_boolean, true)
          end
        end)
      end
    end

    test "right codon, wrong mutations" do
      for mutation <- 3..@max_mutations do
        ActiveMutation.with_mutation({@module, @codon, mutation}, fn ->
          assert OpStrictAndMutator.do_mutate(@module, @codon, true, true) == (true and true)
          assert OpStrictAndMutator.do_mutate(@module, @codon, true, false) == (true and false)
          assert OpStrictAndMutator.do_mutate(@module, @codon, false, true) == (false and true)
          assert OpStrictAndMutator.do_mutate(@module, @codon, false, false) == (false and false)

          assert_raise BadBooleanError, fn ->
            OpStrictAndMutator.do_mutate(@module, @codon, :not_a_boolean, true)
          end
        end)
      end
    end

    test "Mutation 0 - replace with `or`" do
      ActiveMutation.with_mutation({@module, @codon, 0}, fn ->
        assert OpStrictAndMutator.do_mutate(@module, @codon, true, true) == (true or true)
        assert OpStrictAndMutator.do_mutate(@module, @codon, true, false) == (true or false)
        assert OpStrictAndMutator.do_mutate(@module, @codon, false, true) == (false or true)
        assert OpStrictAndMutator.do_mutate(@module, @codon, false, false) == (false or false)

        assert_raise BadBooleanError, fn ->
          OpStrictAndMutator.do_mutate(@module, @codon, :not_a_boolean, true)
        end
      end)
    end

    test "Mutation 1 - replace by `true`" do
      ActiveMutation.with_mutation({@module, @codon, 1}, fn ->
        assert OpStrictAndMutator.do_mutate(@module, @codon, true, true) == true
        assert OpStrictAndMutator.do_mutate(@module, @codon, true, false) == true
        assert OpStrictAndMutator.do_mutate(@module, @codon, false, true) == true
        assert OpStrictAndMutator.do_mutate(@module, @codon, false, false) == true
        assert OpStrictAndMutator.do_mutate(@module, @codon, :not_a_boolean, true) == true
      end)
    end

    test "Mutation 2 - replace by `false`" do
      ActiveMutation.with_mutation({@module, @codon, 2}, fn ->
        assert OpStrictAndMutator.do_mutate(@module, @codon, true, true) == false
        assert OpStrictAndMutator.do_mutate(@module, @codon, true, false) == false
        assert OpStrictAndMutator.do_mutate(@module, @codon, false, true) == false
        assert OpStrictAndMutator.do_mutate(@module, @codon, false, false) == false
        assert OpStrictAndMutator.do_mutate(@module, @codon, :not_a_boolean, true) == false
      end)
    end
  end

  describe "operator: or -" do
    test "wrong module or codon" do
      # If the module and the codon are wrong, the function is compatible
      for module <- @modules,
          codon <- @codons,
          mutation <- 0..@max_mutations,
          {module, codon} != {@module, @codon} do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpStrictOrMutator.do_mutate(@module, @codon, true, true) == (true or true)
          assert OpStrictOrMutator.do_mutate(@module, @codon, true, false) == (true or false)
          assert OpStrictOrMutator.do_mutate(@module, @codon, false, true) == (false or true)
          assert OpStrictOrMutator.do_mutate(@module, @codon, false, false) == (false or false)

          assert_raise BadBooleanError, fn ->
            OpStrictOrMutator.do_mutate(@module, @codon, :not_a_boolean, true)
          end
        end)
      end
    end

    test "right codon, wrong mutations" do
      for mutation <- 3..@max_mutations do
        ActiveMutation.with_mutation({@module, @codon, mutation}, fn ->
          assert OpStrictOrMutator.do_mutate(@module, @codon, true, true) == (true or true)
          assert OpStrictOrMutator.do_mutate(@module, @codon, true, false) == (true or false)
          assert OpStrictOrMutator.do_mutate(@module, @codon, false, true) == (false or true)
          assert OpStrictOrMutator.do_mutate(@module, @codon, false, false) == (false or false)

          assert_raise BadBooleanError, fn ->
            OpStrictOrMutator.do_mutate(@module, @codon, :not_a_boolean, true)
          end
        end)
      end
    end

    test "Mutation 0 - replace with `and`" do
      ActiveMutation.with_mutation({@module, @codon, 0}, fn ->
        assert OpStrictOrMutator.do_mutate(@module, @codon, true, true) == (true and true)
        assert OpStrictOrMutator.do_mutate(@module, @codon, true, false) == (true and false)
        assert OpStrictOrMutator.do_mutate(@module, @codon, false, true) == (false and true)
        assert OpStrictOrMutator.do_mutate(@module, @codon, false, false) == (false and false)

        assert_raise BadBooleanError, fn ->
          OpStrictOrMutator.do_mutate(@module, @codon, :not_a_boolean, true)
        end
      end)
    end

    test "Mutation 1 - replace by `true`" do
      ActiveMutation.with_mutation({@module, @codon, 1}, fn ->
        assert OpStrictOrMutator.do_mutate(@module, @codon, true, true) == true
        assert OpStrictOrMutator.do_mutate(@module, @codon, true, false) == true
        assert OpStrictOrMutator.do_mutate(@module, @codon, false, true) == true
        assert OpStrictOrMutator.do_mutate(@module, @codon, false, false) == true
        assert OpStrictOrMutator.do_mutate(@module, @codon, :not_a_boolean, true) == true
      end)
    end

    test "Mutation 2 - replace by `false`" do
      ActiveMutation.with_mutation({@module, @codon, 2}, fn ->
        assert OpStrictOrMutator.do_mutate(@module, @codon, true, true) == false
        assert OpStrictOrMutator.do_mutate(@module, @codon, true, false) == false
        assert OpStrictOrMutator.do_mutate(@module, @codon, false, true) == false
        assert OpStrictOrMutator.do_mutate(@module, @codon, false, false) == false
        assert OpStrictOrMutator.do_mutate(@module, @codon, :not_a_boolean, true) == false
      end)
    end
  end
end
