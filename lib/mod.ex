defmodule Darwin.KitchenSink do
  @moduledoc false
  import Bitwise

  def comparison_operators(a, b) do
    _ = a < b
    _ = a <= b
    _ = a == b
    _ = a === b
    _ = a !== b
    _ = a != b
    _ = a > b
    _ = a >= b
  end

  def arithmetic_operators(a, b) do
    _x = fn x -> x + 1 end
    _ = a + b
    _ = a - b
    _ = a * b
    _ = a / b
  end

  def strict_boolean_operators(a, b) do
    _ = a and b
    _ = a or b
    _ = not a
  end

  def bitwise_operators(a, b) do
    _ = a >>> b
  end
end
