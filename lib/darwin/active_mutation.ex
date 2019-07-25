defmodule Darwin.ActiveMutation do
  @moduledoc """
  TODO: replace this with an ETS table, which is probably faster.
  """
  use Agent

  def start_link(initial_value) do
    Agent.start_link(fn -> initial_value end, name: __MODULE__)
  end

  def get() do
    Agent.get(__MODULE__, & &1)
  end

  def put(new_value) do
    Agent.update(__MODULE__, fn _ -> new_value end)
  end
end
