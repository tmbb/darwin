defmodule Darwin.MutationServer do
  @moduledoc false
  alias Darwin.ActiveMutation

  @default_mutation {nil, nil, nil}

  @doc """
  Prepares the server for a new test suite
  """
  def start_suite() do
    Application.put_env(:darwin, :next_test_should_run?, true)
  end

  @doc """
  Tell the server that a test has failed.
  From now on, no tests will be run in this test suite.
  """
  def kill_test_suite() do
    Application.put_env(:darwin, :next_test_should_run?, false)
  end

  @doc """
  A test should run if no tests have failed until now.
  """
  def should_this_test_run?() do
    mutation = ActiveMutation.get()
    should_run? = Application.get_env(:darwin, :next_test_should_run?)
    mutation == @default_mutation or should_run? == true
  end
end
