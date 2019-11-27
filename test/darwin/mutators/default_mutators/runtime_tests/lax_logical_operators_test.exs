defmodule Darwin.DefaultMutators.RuntimeTests.LaxLogicalOperatorsTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  alias Darwin.ActiveMutation
  alias DarwinTest.Generators, as: Gen

  alias Darwin.Mutators.Default.{
    OpLaxNotMutator,
    OpLaxAndMutator,
    OpLaxOrMutator
  }

  # ----------------------------------------------------------------
  # TODO: replace hardcoded values by StreamData generators
  # ----------------------------------------------------------------

  property "operator '!': wrong module || codon" do
    # If the module and the codon are wrong, the function is compatible
    check all right_module <- Gen.module(),
              right_codon <- Gen.codon(),
              maybe_wrong_module <- Gen.module(),
              maybe_wrong_codon <- Gen.codon(),
              mutation <- Gen.mutation(),
              {maybe_wrong_module, maybe_wrong_codon} != {right_module, right_codon} do
      ActiveMutation.with_mutation({right_module, right_codon, mutation}, fn ->
        assert OpLaxNotMutator.do_mutate(maybe_wrong_module, maybe_wrong_codon, true) == !true

        assert OpLaxNotMutator.do_mutate(maybe_wrong_module, maybe_wrong_codon, false) == !false

        assert OpLaxNotMutator.do_mutate(maybe_wrong_module, maybe_wrong_codon, :not_a_boolean) ==
                 !:not_a_boolean
      end)
    end
  end

  # TODO: fix this!
  property "operator '!': right codon, wrong mutations" do
    check all right_module <- Gen.module(),
              right_codon <- Gen.codon(),
              maybe_wrong_module <- Gen.module(),
              maybe_wrong_codon <- Gen.codon(),
              mutation <- Gen.mutation(),
              {maybe_wrong_module, maybe_wrong_codon} != {right_module, right_codon} do
      ActiveMutation.with_mutation({right_module, right_codon, mutation}, fn ->
        assert OpLaxNotMutator.do_mutate(maybe_wrong_module, maybe_wrong_codon, true) == not true

        assert OpLaxNotMutator.do_mutate(right_module, maybe_wrong_codon, false) == not false

        assert OpLaxNotMutator.do_mutate(maybe_wrong_module, maybe_wrong_codon, :not_a_boolean) ==
                 !:not_a_boolean
      end)
    end
  end

  property "operator '!': Mutation 0 - remove negation" do
    check all module <- Gen.module(),
              codon <- Gen.codon() do
      ActiveMutation.with_mutation({module, codon, 0}, fn ->
        assert OpLaxNotMutator.do_mutate(module, codon, true) == true
        assert OpLaxNotMutator.do_mutate(module, codon, false) == false
        assert OpLaxNotMutator.do_mutate(module, codon, :other) == :other
      end)
    end
  end

  property "operator '!': Mutation 1 - replace by `true`" do
    check all module <- Gen.module(),
              codon <- Gen.codon() do
      ActiveMutation.with_mutation({module, codon, 1}, fn ->
        assert OpLaxNotMutator.do_mutate(module, codon, true) == true
        assert OpLaxNotMutator.do_mutate(module, codon, false) == true
        assert OpLaxNotMutator.do_mutate(module, codon, :other) == true
      end)
    end
  end

  property "operator '!': Mutation 2 - replace by `false`" do
    check all module <- Gen.module(),
              codon <- Gen.codon() do
      ActiveMutation.with_mutation({module, codon, 2}, fn ->
        assert OpLaxNotMutator.do_mutate(module, codon, true) == false
        assert OpLaxNotMutator.do_mutate(module, codon, false) == false
        assert OpLaxNotMutator.do_mutate(module, codon, :other) == false
      end)
    end
  end

  property "operator '&&': wrong module or codon" do
    check all right_module <- Gen.module(),
              right_codon <- Gen.codon(),
              maybe_wrong_module <- Gen.module(),
              maybe_wrong_codon <- Gen.codon(),
              mutation <- Gen.mutation(),
              {maybe_wrong_module, maybe_wrong_codon} != {right_module, right_codon} do
      ActiveMutation.with_mutation({right_module, right_codon, mutation}, fn ->
        assert OpLaxAndMutator.do_mutate(maybe_wrong_module, maybe_wrong_codon, true, true) ==
                 true

        assert OpLaxAndMutator.do_mutate(maybe_wrong_module, maybe_wrong_codon, true, false) ==
                 false

        assert OpLaxAndMutator.do_mutate(maybe_wrong_module, maybe_wrong_codon, false, true) ==
                 false

        assert OpLaxAndMutator.do_mutate(maybe_wrong_module, maybe_wrong_codon, false, false) ==
                 false

        assert OpLaxAndMutator.do_mutate(
                 maybe_wrong_module,
                 maybe_wrong_codon,
                 :not_a_boolean,
                 true
               ) == true
      end)
    end
  end

  property "operator '&&': right codon, wrong mutations" do
    check all module <- Gen.module(),
              codon <- Gen.codon(),
              mutation <- Gen.mutation(),
              mutation > 2 do
      ActiveMutation.with_mutation({module, codon, mutation}, fn ->
        assert OpLaxAndMutator.do_mutate(module, codon, true, true) == true
        assert OpLaxAndMutator.do_mutate(module, codon, true, false) == false
        assert OpLaxAndMutator.do_mutate(module, codon, false, true) == false
        assert OpLaxAndMutator.do_mutate(module, codon, false, false) == false

        assert OpLaxAndMutator.do_mutate(
                 module,
                 codon,
                 :not_a_boolean,
                 true
               ) == true

        assert OpLaxAndMutator.do_mutate(
                 module,
                 codon,
                 :not_a_boolean,
                 false
               ) == false

        assert OpLaxAndMutator.do_mutate(
                 module,
                 codon,
                 true,
                 :not_a_boolean
               ) == :not_a_boolean

        assert OpLaxAndMutator.do_mutate(
                 module,
                 codon,
                 false,
                 :not_a_boolean
               ) == false
      end)
    end
  end

  property "operator '&&': Mutation 0 - replace with `||`" do
    check all module <- Gen.module(),
              codon <- Gen.codon(),
              mutation = 0 do
      ActiveMutation.with_mutation({module, codon, mutation}, fn ->
        assert OpLaxAndMutator.do_mutate(module, codon, true, true) == true
        assert OpLaxAndMutator.do_mutate(module, codon, true, false) == true
        assert OpLaxAndMutator.do_mutate(module, codon, false, true) == true
        assert OpLaxAndMutator.do_mutate(module, codon, false, false) == false

        assert OpLaxAndMutator.do_mutate(module, codon, :not_a_boolean, true) == :not_a_boolean ||
                 true

        assert OpLaxAndMutator.do_mutate(module, codon, :not_a_boolean, false) == :not_a_boolean ||
                 false

        assert OpLaxAndMutator.do_mutate(module, codon, true, :not_a_boolean) == true ||
                 :not_a_boolean

        assert OpLaxAndMutator.do_mutate(module, codon, false, :not_a_boolean) == false ||
                 :not_a_boolean
      end)
    end
  end

  property "operator '&&': Mutation 1 - replace by `true`" do
    check all module <- Gen.module(),
              codon <- Gen.codon(),
              mutation = 1 do
      ActiveMutation.with_mutation({module, codon, mutation}, fn ->
        assert OpLaxAndMutator.do_mutate(module, codon, true, true) == true
        assert OpLaxAndMutator.do_mutate(module, codon, true, false) == true
        assert OpLaxAndMutator.do_mutate(module, codon, false, true) == true
        assert OpLaxAndMutator.do_mutate(module, codon, false, false) == true

        assert OpLaxAndMutator.do_mutate(module, codon, :not_a_boolean, true) == true
        assert OpLaxAndMutator.do_mutate(module, codon, :not_a_boolean, false) == true
        assert OpLaxAndMutator.do_mutate(module, codon, true, :not_a_boolean) == true
        assert OpLaxAndMutator.do_mutate(module, codon, false, :not_a_boolean) == true
      end)
    end
  end

  property "operator '&&': Mutation 2 - replace by `false`" do
    check all module <- Gen.module(),
              codon <- Gen.codon(),
              mutation = 2 do
      ActiveMutation.with_mutation({module, codon, mutation}, fn ->
        assert OpLaxAndMutator.do_mutate(module, codon, true, true) == false
        assert OpLaxAndMutator.do_mutate(module, codon, true, false) == false
        assert OpLaxAndMutator.do_mutate(module, codon, false, true) == false
        assert OpLaxAndMutator.do_mutate(module, codon, false, false) == false

        assert OpLaxAndMutator.do_mutate(module, codon, :not_a_boolean, true) == false
        assert OpLaxAndMutator.do_mutate(module, codon, :not_a_boolean, false) == false
        assert OpLaxAndMutator.do_mutate(module, codon, true, :not_a_boolean) == false
        assert OpLaxAndMutator.do_mutate(module, codon, false, :not_a_boolean) == false
      end)
    end
  end

  property "operator '||': wrong module or codon" do
    check all right_module <- Gen.module(),
              right_codon <- Gen.codon(),
              maybe_wrong_module <- Gen.module(),
              maybe_wrong_codon <- Gen.codon(),
              mutation <- Gen.mutation(),
              {maybe_wrong_module, maybe_wrong_codon} != {right_module, right_codon} do
      ActiveMutation.with_mutation({right_module, right_codon, mutation}, fn ->
        assert OpLaxOrMutator.do_mutate(maybe_wrong_module, maybe_wrong_codon, true, true) == true

        assert OpLaxOrMutator.do_mutate(maybe_wrong_module, maybe_wrong_codon, true, false) ==
                 true

        assert OpLaxOrMutator.do_mutate(maybe_wrong_module, maybe_wrong_codon, false, true) ==
                 true

        assert OpLaxOrMutator.do_mutate(maybe_wrong_module, maybe_wrong_codon, false, false) ==
                 false

        assert OpLaxOrMutator.do_mutate(
                 maybe_wrong_module,
                 maybe_wrong_codon,
                 :not_a_boolean,
                 true
               ) == :not_a_boolean
      end)
    end
  end

  property "operator '||': right codon, wrong mutations" do
    check all module <- Gen.module(),
              codon <- Gen.codon(),
              mutation <- Gen.mutation(),
              mutation > 2 do
      ActiveMutation.with_mutation({module, codon, mutation}, fn ->
        assert OpLaxOrMutator.do_mutate(module, codon, true, true) == true
        assert OpLaxOrMutator.do_mutate(module, codon, true, false) == true
        assert OpLaxOrMutator.do_mutate(module, codon, false, true) == true
        assert OpLaxOrMutator.do_mutate(module, codon, false, false) == false

        assert OpLaxOrMutator.do_mutate(module, codon, :not_a_boolean, true) == :not_a_boolean
        assert OpLaxOrMutator.do_mutate(module, codon, :not_a_boolean, false) == :not_a_boolean
        assert OpLaxOrMutator.do_mutate(module, codon, true, :not_a_boolean) == true
        assert OpLaxOrMutator.do_mutate(module, codon, false, :not_a_boolean) == :not_a_boolean
      end)
    end
  end

  property "operator '||': Mutation 0 - replace with '&&'" do
    check all module <- Gen.module(),
              codon <- Gen.codon(),
              mutation = 0 do
      ActiveMutation.with_mutation({module, codon, mutation}, fn ->
        assert OpLaxOrMutator.do_mutate(module, codon, true, true) == true
        assert OpLaxOrMutator.do_mutate(module, codon, true, false) == false
        assert OpLaxOrMutator.do_mutate(module, codon, false, true) == false
        assert OpLaxOrMutator.do_mutate(module, codon, false, false) == false

        assert OpLaxOrMutator.do_mutate(module, codon, :not_a_boolean, true) == true

        assert OpLaxOrMutator.do_mutate(module, codon, :not_a_boolean, false) == false

        assert OpLaxOrMutator.do_mutate(
                 module,
                 codon,
                 true,
                 :not_a_boolean
               ) == :not_a_boolean

        assert OpLaxOrMutator.do_mutate(
                 module,
                 codon,
                 false,
                 :not_a_boolean
               ) == false
      end)
    end
  end

  property "operator '||': Mutation 1 - replace by 'true'" do
    check all module <- Gen.module(),
              codon <- Gen.codon(),
              mutation = 1 do
      ActiveMutation.with_mutation({module, codon, mutation}, fn ->
        assert OpLaxOrMutator.do_mutate(module, codon, true, true) == true
        assert OpLaxOrMutator.do_mutate(module, codon, true, false) == true
        assert OpLaxOrMutator.do_mutate(module, codon, false, true) == true
        assert OpLaxOrMutator.do_mutate(module, codon, false, false) == true
        # These would raise an error in the unmutated version
        assert OpLaxOrMutator.do_mutate(module, codon, :not_a_boolean, true) == true
        assert OpLaxOrMutator.do_mutate(module, codon, :not_a_boolean, false) == true
        assert OpLaxOrMutator.do_mutate(module, codon, true, :not_a_boolean) == true
        assert OpLaxOrMutator.do_mutate(module, codon, false, :not_a_boolean) == true
      end)
    end
  end

  property "operator '||': Mutation 2 - replace by 'false'" do
    check all module <- Gen.module(),
              codon <- Gen.codon(),
              mutation = 2 do
      ActiveMutation.with_mutation({module, codon, mutation}, fn ->
        assert OpLaxOrMutator.do_mutate(module, codon, true, true) == false
        assert OpLaxOrMutator.do_mutate(module, codon, true, false) == false
        assert OpLaxOrMutator.do_mutate(module, codon, false, true) == false
        assert OpLaxOrMutator.do_mutate(module, codon, false, false) == false
        # These would raise an error in the unmutated version
        assert OpLaxOrMutator.do_mutate(module, codon, :not_a_boolean, true) == false
        assert OpLaxOrMutator.do_mutate(module, codon, :not_a_boolean, false) == false
        assert OpLaxOrMutator.do_mutate(module, codon, true, :not_a_boolean) == false
        assert OpLaxOrMutator.do_mutate(module, codon, false, :not_a_boolean) == false
      end)
    end
  end
end
