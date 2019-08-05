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

  @spec mutation_index_for_codon(atom(), integer()) :: {:ok, integer()} | :error
  def mutation_index_for_codon(module, codon_index) do
    # To make it easier to test the library, we'll use a little hack.
    # There will be a special key in the process dictionary, so that we can feed some mutations
    # without setting them globally for all projects.
    # This makes sure there won't be any race conditions, with little runtime cost.
    # The call to `:erlang.get/1` to get the key from the process dictionary and the comparison
    # to `:undefined` take about 500ns, which seems worth it for convenience.
    # `:erlang.get/1` is flightly faster than `Process.get/2` in my benchmarks (~ 20ns faster)
    #
    # TODO: Does this really make sense?!
    {active_module, active_codon_index, active_mutation_index} =
      case :erlang.get(:"$darwin.active_mutation") do
        :undefined ->
          get()

        trio ->
          trio
      end

    if module == active_module and codon_index == active_codon_index do
      {:ok, active_mutation_index}
    else
      :error
    end
  end

  def with_mutation(mutation_id, fun) do
    # This function uses the hack above to set the mutations "locally" in the current process
    Process.put(:"$darwin.active_mutation", mutation_id)

    # Store the result to return it later
    result =
      try do
        fun.()
      rescue
        exception ->
          # Clean the process dictionary and raise the exception
          Process.delete(:"$darwin.active_mutation")
          raise exception
      end

    # Clean the process dictionary
    Process.delete(:...)
    # Now that we're done, return the result
    result
  end

  def put(new_value) do
    Agent.update(__MODULE__, fn _ -> new_value end)
  end
end
