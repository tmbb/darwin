defmodule Darwin do
  @moduledoc """
  Mutation testing framework.
  """

  @doc """
  Tests whether the Darwin test suite is running.
  """
  def running?() do
    if Application.get_env(:darwin, :darwin_is_running?), do: true, else: false
  end
end
