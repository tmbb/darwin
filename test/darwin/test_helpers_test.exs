defmodule Darwin.TestHelpersTest do
  use ExUnit.Case, async: true
  import Darwin.TestHelpers

  defp zero(), do: 0
  defp division_by_zero(), do: 1 / zero()

  test "assert_equivalent/2" do
    assert assert_equivalent(2, 1 + 1) == :ok

    # LHS raises an error; RHS doesn't
    assert_raise KeyError, fn ->
      assert_equivalent(Keyword.fetch!([a: 1], :b), 1 + 1)
    end

    # RHS raises an error; LHS doesn't
    assert_raise KeyError, fn ->
      assert_equivalent(6 + zero(), Keyword.fetch!([a: 1], :b))
    end

    assert assert_equivalent(
             Keyword.fetch!([a: 1], :b),
             Keyword.fetch!([a: 1], :c)
           ) == :ok

    # Both sides raise an different errors
    assert_raise ExUnit.AssertionError, fn ->
      assert_equivalent(
        division_by_zero(),
        Keyword.fetch!([a: 1], :b)
      )
    end
  end

  # test "<~>/2 (which is just an abbreviation for assert_equivalent/2)" do
  #   assert assert_equivalent(2, 1 + 1) == true

  #   # LHS raises an error; RHS doesn't
  #   assert_raise ArithmeticError, fn ->
  #     raise(ArithmeticError) <~> 1 + 1
  #   end

  #   # RHS raises an error; LHS doesn't
  #   assert_raise ArithmeticError, fn ->
  #     :ok <~> 1 / raise(ArithmeticError)
  #   end

  #   1 / (fn -> :ok; 0 end).() <~> 2 + (fn -> :ok; :error end).()

  #   # Both sides raise an different errors
  #   assert_raise ExUnit.AssertionError, fn ->
  #     raise(ArithmeticError) <~> raise(ArgumentError)
  #   end
  # end
end
