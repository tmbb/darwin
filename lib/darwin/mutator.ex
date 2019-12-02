defmodule Darwin.Mutator do
  alias Darwin.Mutator.Context
  alias Darwin.Mutators.Default
  alias Darwin.Erlang
  alias Darwin.Erlang.AbstractCode
  alias Darwin.Beam

  @type mutator_result() :: {:ok, {AbstractCode.t(), Context.t()}} | :error

  @type mutator() :: atom()

  @callback mutate(AbstractCode.t(), Context.t()) :: nil

  def call_mutator(
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
  Mutates Erlang abstract code.
  """
  @spec mutate(AbstractCode.t(), atom(), list(mutator())) :: mutator_result()
  def mutate(abstract_code, module, mutators \\ Default.mutators()) do
    ctx = Context.new(module: module, mutators: mutators)
    do_mutate(abstract_code, ctx)
  end

  @doc """
  Mutates the module with the given name.
  """
  def mutate_module(module, mutators \\ Default.mutators()) do
    form_list = Beam.beam_to_abstract_code(module)
    mutate(form_list, module, mutators)
  end

  @debug_path "darwin_debug"

  @doc """
  Mutates the module with the given name and makes it available to the BEAM runtime.
  """
  def mutate_compile_and_load_module(module_name, opts \\ []) do
    debug = Keyword.get(opts, :debug, true)

    {mutated_form_list, ctx} = mutate_module(module_name)
    Beam.compile_and_load(mutated_form_list)

    if debug do
      File.mkdir(@debug_path)
      contents = Erlang.pprint_forms(mutated_form_list)
      File.write!(Path.join(@debug_path, to_string(module_name) <> ".erl"), contents)
    end

    ctx
  end

  @doc """
  Apply the mutators in order until one of them matches.

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

  @doc false
  def make_mutation_opts(abstract_code, mutator, mutation) do
    {m, f, args} = Keyword.fetch!(mutation, :abstract_code_mfa)
    mutated_abstract_code = apply(m, f, args ++ [abstract_code])

    mutation
    |> Keyword.put(:mutated_codon, mutated_abstract_code)
    |> Keyword.put(:mutator, mutator)
  end

  defp make_case_clauses(default_mfa, alternatives, op_args) do
    clauses =
      for {alternative, index} <- Enum.with_index(alternatives, 0) do
        quoted_mfa = Keyword.fetch!(alternative, :runtime_mfa)

        quote do
          unquote(index) ->
            {alt_m, alt_f, alt_args} = unquote(quoted_mfa)
            apply(alt_m, alt_f, alt_args ++ unquote(op_args))
        end
      end
      |> List.flatten()

    default_clause =
      quote do
        _ ->
          {default_m, default_f, default_args} = unquote(default_mfa)
          apply(default_m, default_f, default_args ++ unquote(op_args))
      end

    clauses ++ default_clause
  end
end
