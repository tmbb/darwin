defmodule Mod do
  @moduledoc """
  Dummy module just to make it easy to inspect some erlang expressions
  """
  def f(a) do
    <<g(a)>>
  end

  def g(x), do: x
end
