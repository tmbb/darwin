defmodule Darwin.TestRunner.Formatter do
  @doc false
  use GenServer
  alias Darwin.MutationServer

  #  The following events are possible:
  #   {:suite_started, opts} - the suite has started with the specified options to the runner.
  #   {:suite_finished, run_us, load_us} - the suite has finished. run_us and load_us are the run and load times in microseconds respectively.
  #   {:module_started, test_module} - a test module has started. See ExUnit.TestModule for details.
  #   {:module_finished, test_module} - a test module has finished. See ExUnit.TestModule for details.
  #   {:test_started, test} - a test has started. See ExUnit.Test for details.
  #   {:test_finished, test} - a test has finished. See ExUnit.Test for details.

  @impl true
  def init(_config) do
    {:ok, []}
  end

  # We only care about the tests that fail
  @impl true
  def handle_cast({:test_finished, %{state: {:failed, _failed}} = _test}, state) do
    MutationServer.kill_test_suite()
    {:noreply, state}
  end

  # These clauses don't do anything
  def handle_cast(_, state) do
    {:noreply, state}
  end
end
