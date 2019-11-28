defmodule Darwin do
  @moduledoc """
  Mutation testing framework.
  """

  @darwin_is_running_key :darwin_is_running?

  @doc """
  Tests whether the Darwin test suite is running.
  """
  def running?() do
    if Application.get_env(:darwin, @darwin_is_running_key), do: true, else: false
  end

  @doc false
  def darwin_is_now_running() do
    Application.put_env(:darwin, @darwin_is_running_key, true)
  end

  @doc false
  def darwin_has_stopped() do
    Application.put_env(:darwin, @darwin_is_running_key, false)
  end
end
