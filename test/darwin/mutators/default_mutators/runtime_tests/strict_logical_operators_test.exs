defmodule Darwin.DefaultMutators.RuntimeTests.StrictLogicalOperatorsTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  alias Darwin.ActiveMutation
  alias Darwin.TestGenerators, as: Gen

  alias Darwin.Mutators.Default.{
    OpStrictNotMutator,
    OpStrictAndMutator,
    OpStrictOrMutator
  }

  # ----------------------------------------------------------------
  # TODO: replace hardcoded values by StreamData generators;
  # Be careful because these operators raise errors for some values
  # ----------------------------------------------------------------

  describe "operator 'not':" do
    property "wrong module or codon" do
      # If the module and the codon are wrong, the function is compatible
      check all(
              right_module <- Gen.module(),
              right_codon <- Gen.codon(),
              maybe_wrong_module <- Gen.module(),
              maybe_wrong_codon <- Gen.codon(),
              mutation <- Gen.mutation(),
              {maybe_wrong_module, maybe_wrong_codon} != {right_module, right_codon}
            ) do
        ActiveMutation.with_mutation({right_module, right_codon, mutation}, fn ->
          assert OpStrictNotMutator.darwin_was_here(maybe_wrong_module, maybe_wrong_codon, true) ==
                   not true

          assert OpStrictNotMutator.darwin_was_here(maybe_wrong_module, maybe_wrong_codon, false) ==
                   not false

          assert_raise ArgumentError, fn ->
            OpStrictNotMutator.darwin_was_here(
              maybe_wrong_module,
              maybe_wrong_codon,
              :not_a_boolean
            )
          end
        end)
      end
    end

    # TODO: fix this!
    property "right codon, wrong mutations" do
      check all(
              right_module <- Gen.module(),
              right_codon <- Gen.codon(),
              maybe_wrong_module <- Gen.module(),
              maybe_wrong_codon <- Gen.codon(),
              mutation <- Gen.mutation(),
              {maybe_wrong_module, maybe_wrong_codon} != {right_module, right_codon}
            ) do
        ActiveMutation.with_mutation({right_module, right_codon, mutation}, fn ->
          assert OpStrictNotMutator.darwin_was_here(maybe_wrong_module, maybe_wrong_codon, true) ==
                   not true

          assert OpStrictNotMutator.darwin_was_here(right_module, maybe_wrong_codon, false) ==
                   not false

          assert_raise ArgumentError, fn ->
            OpStrictNotMutator.darwin_was_here(
              maybe_wrong_module,
              maybe_wrong_codon,
              :not_a_boolean
            )
          end
        end)
      end
    end

    property "Mutation 0 - remove negation" do
      check all(
              module <- Gen.module(),
              codon <- Gen.codon()
            ) do
        ActiveMutation.with_mutation({module, codon, 0}, fn ->
          assert OpStrictNotMutator.darwin_was_here(module, codon, true) == true
          assert OpStrictNotMutator.darwin_was_here(module, codon, false) == false
          assert OpStrictNotMutator.darwin_was_here(module, codon, :other) == :other
        end)
      end
    end

    property "Mutation 1 - replace by `true`" do
      check all(
              module <- Gen.module(),
              codon <- Gen.codon()
            ) do
        ActiveMutation.with_mutation({module, codon, 1}, fn ->
          assert OpStrictNotMutator.darwin_was_here(module, codon, true) == true
          assert OpStrictNotMutator.darwin_was_here(module, codon, false) == true
          assert OpStrictNotMutator.darwin_was_here(module, codon, :other) == true
        end)
      end
    end

    property "Mutation 2 - replace by `false`" do
      check all(
              module <- Gen.module(),
              codon <- Gen.codon()
            ) do
        ActiveMutation.with_mutation({module, codon, 2}, fn ->
          assert OpStrictNotMutator.darwin_was_here(module, codon, true) == false
          assert OpStrictNotMutator.darwin_was_here(module, codon, false) == false
          assert OpStrictNotMutator.darwin_was_here(module, codon, :other) == false
        end)
      end
    end
  end

  describe "operator 'and':" do
    property "wrong module or codon" do
      check all(
              right_module <- Gen.module(),
              right_codon <- Gen.codon(),
              maybe_wrong_module <- Gen.module(),
              maybe_wrong_codon <- Gen.codon(),
              mutation <- Gen.mutation(),
              {maybe_wrong_module, maybe_wrong_codon} != {right_module, right_codon}
            ) do
        ActiveMutation.with_mutation({right_module, right_codon, mutation}, fn ->
          assert OpStrictAndMutator.darwin_was_here(
                   maybe_wrong_module,
                   maybe_wrong_codon,
                   true,
                   true
                 ) == (true and true)

          assert OpStrictAndMutator.darwin_was_here(
                   maybe_wrong_module,
                   maybe_wrong_codon,
                   true,
                   false
                 ) == (true and false)

          assert OpStrictAndMutator.darwin_was_here(
                   maybe_wrong_module,
                   maybe_wrong_codon,
                   false,
                   true
                 ) == (false and true)

          assert OpStrictAndMutator.darwin_was_here(
                   maybe_wrong_module,
                   maybe_wrong_codon,
                   false,
                   false
                 ) == (false and false)

          assert_raise BadBooleanError, fn ->
            OpStrictAndMutator.darwin_was_here(
              maybe_wrong_module,
              maybe_wrong_codon,
              :not_a_boolean,
              true
            )
          end
        end)
      end
    end

    property "right codon, wrong mutations" do
      check all(
              module <- Gen.module(),
              codon <- Gen.codon(),
              mutation <- Gen.mutation(),
              mutation > 2
            ) do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpStrictAndMutator.darwin_was_here(module, codon, true, true) == (true and true)

          assert OpStrictAndMutator.darwin_was_here(module, codon, true, false) ==
                   (true and false)

          assert OpStrictAndMutator.darwin_was_here(module, codon, false, true) ==
                   (false and true)

          assert OpStrictAndMutator.darwin_was_here(module, codon, false, false) ==
                   (false and false)

          assert_raise BadBooleanError, fn ->
            OpStrictAndMutator.darwin_was_here(module, codon, :not_a_boolean, true)
            OpStrictAndMutator.darwin_was_here(module, codon, :not_a_boolean, false)
            OpStrictAndMutator.darwin_was_here(module, codon, true, :not_a_boolean)
            OpStrictAndMutator.darwin_was_here(module, codon, false, :not_a_boolean)
          end
        end)
      end
    end

    property "Mutation 0 - replace with `or`" do
      check all(
              module <- Gen.module(),
              codon <- Gen.codon(),
              mutation = 0
            ) do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpStrictAndMutator.darwin_was_here(module, codon, true, true) == (true or true)
          assert OpStrictAndMutator.darwin_was_here(module, codon, true, false) == (true or false)
          assert OpStrictAndMutator.darwin_was_here(module, codon, false, true) == (false or true)

          assert OpStrictAndMutator.darwin_was_here(module, codon, false, false) ==
                   (false or false)

          assert_raise BadBooleanError, fn ->
            OpStrictAndMutator.darwin_was_here(module, codon, :not_a_boolean, true)
            OpStrictAndMutator.darwin_was_here(module, codon, :not_a_boolean, false)
            OpStrictAndMutator.darwin_was_here(module, codon, true, :not_a_boolean)
            OpStrictAndMutator.darwin_was_here(module, codon, false, :not_a_boolean)
          end
        end)
      end
    end

    property "Mutation 1 - replace by `true`" do
      check all(
              module <- Gen.module(),
              codon <- Gen.codon(),
              mutation = 1
            ) do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpStrictAndMutator.darwin_was_here(module, codon, true, true) == true
          assert OpStrictAndMutator.darwin_was_here(module, codon, true, false) == true
          assert OpStrictAndMutator.darwin_was_here(module, codon, false, true) == true
          assert OpStrictAndMutator.darwin_was_here(module, codon, false, false) == true
          # These would raise an error in the unmutated version
          assert OpStrictAndMutator.darwin_was_here(module, codon, :not_a_boolean, true) == true
          assert OpStrictAndMutator.darwin_was_here(module, codon, :not_a_boolean, false) == true
          assert OpStrictAndMutator.darwin_was_here(module, codon, true, :not_a_boolean) == true
          assert OpStrictAndMutator.darwin_was_here(module, codon, false, :not_a_boolean) == true
        end)
      end
    end

    property "Mutation 2 - replace by `false`" do
      check all(
              module <- Gen.module(),
              codon <- Gen.codon(),
              mutation = 2
            ) do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpStrictAndMutator.darwin_was_here(module, codon, true, true) == false
          assert OpStrictAndMutator.darwin_was_here(module, codon, true, false) == false
          assert OpStrictAndMutator.darwin_was_here(module, codon, false, true) == false
          assert OpStrictAndMutator.darwin_was_here(module, codon, false, false) == false
          # These would raise an error in the unmutated version
          assert OpStrictAndMutator.darwin_was_here(module, codon, :not_a_boolean, true) == false
          assert OpStrictAndMutator.darwin_was_here(module, codon, :not_a_boolean, false) == false
          assert OpStrictAndMutator.darwin_was_here(module, codon, true, :not_a_boolean) == false
          assert OpStrictAndMutator.darwin_was_here(module, codon, false, :not_a_boolean) == false
        end)
      end
    end
  end

  describe "operator 'or':" do
    property "wrong module or codon" do
      check all(
              right_module <- Gen.module(),
              right_codon <- Gen.codon(),
              maybe_wrong_module <- Gen.module(),
              maybe_wrong_codon <- Gen.codon(),
              mutation <- Gen.mutation(),
              {maybe_wrong_module, maybe_wrong_codon} != {right_module, right_codon}
            ) do
        ActiveMutation.with_mutation({right_module, right_codon, mutation}, fn ->
          assert OpStrictOrMutator.darwin_was_here(
                   maybe_wrong_module,
                   maybe_wrong_codon,
                   true,
                   true
                 ) == (true or true)

          assert OpStrictOrMutator.darwin_was_here(
                   maybe_wrong_module,
                   maybe_wrong_codon,
                   true,
                   false
                 ) == (true or false)

          assert OpStrictOrMutator.darwin_was_here(
                   maybe_wrong_module,
                   maybe_wrong_codon,
                   false,
                   true
                 ) == (false or true)

          assert OpStrictOrMutator.darwin_was_here(
                   maybe_wrong_module,
                   maybe_wrong_codon,
                   false,
                   false
                 ) == (false or false)

          assert_raise BadBooleanError, fn ->
            OpStrictOrMutator.darwin_was_here(
              maybe_wrong_module,
              maybe_wrong_codon,
              :not_a_boolean,
              true
            )
          end
        end)
      end
    end

    property "right codon, wrong mutations" do
      check all(
              module <- Gen.module(),
              codon <- Gen.codon(),
              mutation <- Gen.mutation(),
              mutation > 2
            ) do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpStrictOrMutator.darwin_was_here(module, codon, true, true) == (true or true)
          assert OpStrictOrMutator.darwin_was_here(module, codon, true, false) == (true or false)
          assert OpStrictOrMutator.darwin_was_here(module, codon, false, true) == (false or true)

          assert OpStrictOrMutator.darwin_was_here(module, codon, false, false) ==
                   (false or false)

          assert_raise BadBooleanError, fn ->
            OpStrictOrMutator.darwin_was_here(module, codon, :not_a_boolean, true)
            OpStrictOrMutator.darwin_was_here(module, codon, :not_a_boolean, false)
            OpStrictOrMutator.darwin_was_here(module, codon, true, :not_a_boolean)
            OpStrictOrMutator.darwin_was_here(module, codon, false, :not_a_boolean)
          end
        end)
      end
    end

    test "Mutation 0 - replace with 'and'" do
      check all(
              module <- Gen.module(),
              codon <- Gen.codon(),
              mutation = 0
            ) do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpStrictOrMutator.darwin_was_here(module, codon, true, true) == (true and true)
          assert OpStrictOrMutator.darwin_was_here(module, codon, true, false) == (true and false)
          assert OpStrictOrMutator.darwin_was_here(module, codon, false, true) == (false and true)

          assert OpStrictOrMutator.darwin_was_here(module, codon, false, false) ==
                   (false and false)

          assert_raise BadBooleanError, fn ->
            OpStrictOrMutator.darwin_was_here(module, codon, :not_a_boolean, true)
            OpStrictOrMutator.darwin_was_here(module, codon, :not_a_boolean, false)
            OpStrictOrMutator.darwin_was_here(module, codon, true, :not_a_boolean)
            OpStrictOrMutator.darwin_was_here(module, codon, false, :not_a_boolean)
          end
        end)
      end
    end

    property "Mutation 1 - replace by 'true'" do
      check all(
              module <- Gen.module(),
              codon <- Gen.codon(),
              mutation = 1
            ) do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpStrictOrMutator.darwin_was_here(module, codon, true, true) == true
          assert OpStrictOrMutator.darwin_was_here(module, codon, true, false) == true
          assert OpStrictOrMutator.darwin_was_here(module, codon, false, true) == true
          assert OpStrictOrMutator.darwin_was_here(module, codon, false, false) == true
          # These would raise an error in the unmutated version
          assert OpStrictOrMutator.darwin_was_here(module, codon, :not_a_boolean, true) == true
          assert OpStrictOrMutator.darwin_was_here(module, codon, :not_a_boolean, false) == true
          assert OpStrictOrMutator.darwin_was_here(module, codon, true, :not_a_boolean) == true
          assert OpStrictOrMutator.darwin_was_here(module, codon, false, :not_a_boolean) == true
        end)
      end
    end

    property "Mutation 2 - replace by 'false'" do
      check all(
              module <- Gen.module(),
              codon <- Gen.codon(),
              mutation = 2
            ) do
        ActiveMutation.with_mutation({module, codon, mutation}, fn ->
          assert OpStrictOrMutator.darwin_was_here(module, codon, true, true) == false
          assert OpStrictOrMutator.darwin_was_here(module, codon, true, false) == false
          assert OpStrictOrMutator.darwin_was_here(module, codon, false, true) == false
          assert OpStrictOrMutator.darwin_was_here(module, codon, false, false) == false
          # These would raise an error in the unmutated version
          assert OpStrictOrMutator.darwin_was_here(module, codon, :not_a_boolean, true) == false
          assert OpStrictOrMutator.darwin_was_here(module, codon, :not_a_boolean, false) == false
          assert OpStrictOrMutator.darwin_was_here(module, codon, true, :not_a_boolean) == false
          assert OpStrictOrMutator.darwin_was_here(module, codon, false, :not_a_boolean) == false
        end)
      end
    end
  end
end
