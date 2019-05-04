defmodule Mod do
  @moduledoc """
  Dummy module just to make it easy to inspect some erlang expressions
  """
  def f(a, b) do
    _ = a || b
    _ = a && b
    _ = a > b
    _ = a >= b
    _ = a == b
    _ = a != b
    _ = a <= b
    _ = a < b
  end
end
