defmodule Darwin.ActiveMutation do
  @moduledoc """
  TODO: replace this with an ETS table, which is probably faster.
  """

  @default_mutation {nil, nil, nil}

  @pdict_key :"$darwin.active_mutation"

  @doc """
  Get the current active mutation.

  Returns a tuple (not a `%Mutation{}`).
  """
  def get() do
    Application.get_env(:darwin, :active_mutation, @default_mutation)
  end

  @doc """
  Set the current active mutation.

  Expects a `%Mutation{}` (not a tuple).
  """
  def put(mutation) do
    tuple = {mutation.module, mutation.original_codon_index, mutation.index}
    Application.put_env(:darwin, :active_mutation, tuple)
  end

  @doc """
  Delete the current active mutation (set it to the default).
  """
  def delete() do
    Application.put_env(:darwin, :active_mutation, @default_mutation)
  end

  @spec mutation_index_for_codon(atom(), integer()) :: {:ok, integer()} | :error
  def mutation_index_for_codon(module, codon_index) do
    # To make it easier to test the library, we'll use a little hack.
    # There will be a special key in the process dictionary, so that we can feed some mutations
    # without setting them globally for all projects.
    # This makes sure there won't be any race conditions, with little runtime cost.
    # The call to `:erlang.get/1` to get the key from the process dictionary and the comparison
    # to `:undefined` take about 500ns, which seems worth it for convenience.
    # `:erlang.get/1` is slightly faster than `Process.get/2` in my benchmarks (~ 20ns faster)
    #
    # TODO: Does this micro-optimization really make sense?!
    {active_module, active_codon_index, active_mutation_index} =
      case :erlang.get(@pdict_key) do
        :undefined ->
          get()

        tuple ->
          tuple
      end

    if module == active_module and codon_index == active_codon_index do
      {:ok, active_mutation_index}
    else
      :error
    end
  end

  def with_mutation(mutation_id, fun) do
    # This function uses the hack above to set the mutations "locally" in the current process
    Process.put(@pdict_key, mutation_id)

    # Store the result to return it later
    result =
      try do
        fun.()
      after
        # Clean the process dictionary and raise the exception
        Process.delete(@pdict_key)
      end

    # Clean the process dictionary
    Process.delete(@pdict_key)
    # Now that we're done, return the result
    result
  end
end
