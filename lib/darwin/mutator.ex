defmodule Darwin.Mutator do
  alias Darwin.Mutator.Context
  alias Darwin.Mutators.Default
  alias Darwin.Erlang.AbstractCode
  alias Darwin.Beam

  @type mutator_result() :: {:ok, {AbstractCode.t(), Context.t()}} | :error

  @type mutator() :: atom()

  @callback mutate(AbstractCode.t(), Context.t()) :: mutator_result()

  @doc """
  Returns the Erlang abstract code for calling function `fun`
  (given as a `{remote_module, function_name}` pair) on a certain `codon`
  (given as a `{module, codon_index}` pair) with the given `args`
  on the given `line`.

  It's meant to be used as a helper to call the `darwin_was_here()`
  function of the mutator module.

  ## Examples

      defmodule MyMutator do
        @behaviour Darwin.Mutator
        alias Darwin.Mutator
        # ...

        def mutate(..., ctx) do
          # ...
          mutated_abstract_code =
            Mutator.mutation_for_codon(
              # remote function
              {__MODULE__, :darwin_was_here},
              # codon
              {module, codon_index},
              # other function arguments
              [mutated_left, mutated_right],
              # line number
              line
            )
          # ...
        end

        # Public (because it has to be invoked outside this module)
        # but not accessible in the documentation
        @doc false
        def darwin_was_here(module, codon_index, left, right) do
          # ...
        end
      end
  """
  def mutation_for_codon(
        {caller_module, helper_name} = _fun,
        {module, codon_index} = _codon,
        args,
        line
      ) do
    codon_args = [
      AbstractCode.encode_atom(module, line: 0),
      AbstractCode.encode_integer(codon_index, line: 0)
    ]

    AbstractCode.call_mfa(
      {caller_module, helper_name, codon_args ++ args},
      line: line
    )
  end

  @doc """
  Mutates Erlang abstract code for the given module.
  """
  @spec mutate(AbstractCode.t(), atom(), list(mutator())) :: mutator_result()
  def mutate(abstract_code, module, mutators \\ Default.mutators()) do
    ctx = Context.new(module: module, mutators: mutators)
    do_mutate(abstract_code, ctx)
  end

  @doc """
  Mutates the module with the given name.
  Doesn't take the abstract code as an argument.
  """
  def mutate_module(module, mutators \\ Default.mutators()) do
    form_list = Beam.beam_to_abstract_code(module)
    mutate(form_list, module, mutators)
  end

  @doc """
  Mutates the module with the given name and makes it available to the BEAM runtime.
  """
  def mutate_compile_and_load_module(module_name) do
    {mutated_form_list, ctx} = mutate_module(module_name)
    Beam.compile_and_load(mutated_form_list)
    ctx
  end

  @doc """
  Apply the mutators in order until one of them matches.
  It returns the function of the matched mutator.

  A mutator `m` matches if `m.mutate/3` returns an {:ok, {abstract_code, ctx}} tuple.
  """
  @spec mutate(AbstractCode.t(), Context.t()) :: mutator_result()
  def do_mutate(abstract_code, ctx) do
    %{mutators: mutators} = ctx

    Enum.reduce_while(mutators, {abstract_code, ctx}, fn mutator, {abstract_code, ctx} ->
      case mutator.mutate(abstract_code, ctx) do
        {:ok, {new_abstract_code, new_ctx}} -> {:halt, {new_abstract_code, new_ctx}}
        :error -> {:cont, {abstract_code, ctx}}
      end
    end)
  end

  def do_map_mutate(list, ctx) do
    {mutated_reversed_list, ctx} =
      Enum.reduce(list, {[], ctx}, fn abstract_code, {acc, ctx} ->
        {mutated_abstract_code, ctx} = do_mutate(abstract_code, ctx)
        {[mutated_abstract_code | acc], ctx}
      end)

    {:lists.reverse(mutated_reversed_list), ctx}
  end
end
