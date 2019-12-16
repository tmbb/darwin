defmodule Darwin.ActiveMutation do
  @moduledoc """
  Utilities to get and set the active mutation at runtime.
  """

  @default_mutation {nil, nil, nil}

  @pdict_key :"$darwin.active_mutation"

  @doc """
  Get the current active mutation. Returns a tuple (not a `%Mutation{}`).

  Probably not meant to be used by end users directly except when testing mutators.
  Use `Darwin.ActiveMutation.mutation_index_for_codon/2` instead in your mutators.
  """
  def get() do
    Application.get_env(:darwin, :active_mutation, @default_mutation)
  end

  @doc """
  Set the current active mutation. Expects a `%Mutation{}` (not a tuple).

  Probably not meant to be used by end users directly except when testing mutators.
  For testing purposes, you should probably use `Darwin.ActiveMutation.with_mutation/2`
  which sets the mutation only for the current process and allows you to test
  several mutations in parallel.
  """
  def put(mutation) do
    tuple = {mutation.module, mutation.original_codon_index, mutation.index}
    Application.put_env(:darwin, :active_mutation, tuple)
  end

  @doc """
  Delete the current active mutation (set it to the default).

  Probably not meant to be used by end users directly except when testing mutators.
  For testing purposes, you should probably use `Darwin.ActiveMutation.with_mutation/2`
  which sets the mutation only for the current process and allows you to test
  several mutations in parallel.
  """
  def delete() do
    Application.put_env(:darwin, :active_mutation, @default_mutation)
  end

  @doc """
  Get the mutation index for the given codon.
  If the current active mutation belongs to the codon, it returns `{:ok, index}`.
  Otherwise, it returns `:error`.

  As usual, a codon is identified uniquely by a module name and by a index.
  The mutation belongs to the codon if the module and the codon index are the same.

  This function is useful to select which code to run based on the mutation index.
  If the mutation doesn't belong to the current codon, you want to run the original code.
  If the mutation belongs to the current codon, we want to pick which code to run
  based on the mutation index.

  ## Examples

      @doc false
      def darwin_was_here(module, codon_index, left, right) do
        case ActiveMutation.mutation_index_for_codon(module, codon_index) do
          {:ok, 0} -> left || right
          {:ok, 1} -> true
          {:ok, 2} -> false
          _ -> left && right
        end
      end
  """
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

  @doc """
  Runs a function with a given mutation.
  Expects a tuple representing a mutation and an anonymous function.

  Sets the mutation locally for the current process, not globally.
  Useful to test mutations in parallel.
  It  can be used with ExUnit in test cases with `async: true`.

  ## Examples

      defmodule Darwin.DefaultMutators.RuntimeTests.ArithmeticOperatorsTest do
        use ExUnit.Case, async: true
        use ExUnitProperties
        alias DarwinTest.Generators, as: Gen
        alias Darwin.ActiveMutation

        property "Mutation 0 - replace by '+'" do
          # Use StreamData to generate some custom inputs
          check all module <- Gen.module(),
                    codon <- Gen.codon(),
                    mutation = 0,
                    a <- one_of([float(), integer()]),
                    b <- one_of([float(), integer()]) do
            # Run the anonymous function with the given mutation
            # The mutation is active for this process only
            # which means we can run other tests in parallel
            ActiveMutation.with_mutation({module, codon, mutation}, fn ->
              assert OpSubMutator.darwin_was_here(module, codon, a, b) == a + b
            end)
          end
        end
      end
  """
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
