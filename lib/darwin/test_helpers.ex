defmodule Darwin.TestHelpers do
  alias Darwin.Mutator
  alias Darwin.Erlang
  alias Darwin.ExToErl
  alias Darwin.Beam
  alias Darwin.ActiveMutation
  alias Darwin.Mutators.Default
  require ExUnit.Assertions
  import ExUnit.Assertions, only: [assert: 2]

  @doc """
  Helpers to make it easier to mutate Elixir and Erlang expressions
  """

  def mutate_erlang(bin, opts \\ [module: MyModule]) do
    module = Keyword.fetch!(opts, :module)

    bin
    |> Erlang.expression!()
    |> Mutator.mutate(module)
  end

  def mutate_elixir(bin, opts \\ [module: MyModule]) do
    module = Keyword.fetch!(opts, :module)

    bin
    |> ExToErl.elixir_source_to_erlang_abstract_code()
    |> Mutator.mutate(module)
  end

  def compile_elixir(arg, opts \\ [module: MyModule])

  def compile_elixir(bin, opts) when is_binary(bin) do
    module = Keyword.fetch!(opts, :module)
    body = Code.string_to_quoted!(bin)
    Module.create(module, body, Macro.Env.location(__ENV__))
  end

  def compile_elixir(body, opts) do
    module = Keyword.fetch!(opts, :module)
    Module.create(module, body, Macro.Env.location(__ENV__))
  end

  def mutate_and_compile_elixir(arg, opts \\ [module: MyModule])

  def mutate_and_compile_elixir(bin, opts) when is_binary(bin) do
    body = Code.string_to_quoted!(bin)
    mutate_and_compile_elixir(body, opts)
  end

  require Logger

  def mutate_and_compile_elixir(body, opts) do
    module = Keyword.fetch!(opts, :module)
    mutators = Keyword.get(opts, :mutators, Default.mutators())

    {:module, _, chunks, _} = compile_elixir(body, opts)

    {:ok, {_, [{:abstract_code, {_, abstract_code}}]}} =
      :beam_lib.chunks(chunks, [:abstract_code])

    {mutated_abstract_code, ctx} = Mutator.mutate(abstract_code, module, mutators)

    Beam.compile_and_load(mutated_abstract_code)

    {mutated_abstract_code, ctx}
  end

  def mutate_elixir_module(module_name, body, _opts \\ []) do
    module_name
    |> ExToErl.elixir_module_source_to_erlang_abstract_code(body)
    |> Mutator.mutate(module_name)
  end

  defp replace_by(
         {
           {:., _, [Access, :get]},
           [],
           [
             {:__aliases__, _, [:MUTATION]},
             {:>>>, _, [left, right]}
           ]
         },
         side
       ) do
    result =
      case side do
        :left -> left
        :right -> right
      end

    {result, true}
  end

  defp replace_by(other, _side), do: {other, false}

  defp replace_ast_and_count(ast, nr_of_mutations, side) do
    {result, found_mutation?} = replace_by(ast, side)

    new_nr_of_mutations =
      case found_mutation? do
        true -> nr_of_mutations + 1
        false -> nr_of_mutations
      end

    {result, new_nr_of_mutations}
  end

  @doc false
  def debug_ast(ast, label) do
    code =
      ast
      |> Macro.to_string()
      |> Code.format_string!()

    IO.puts("")
    IO.puts(label <> ":")
    IO.puts(String.duplicate("-", byte_size(label) + 1))
    IO.puts(code)
    IO.puts("")
  end

  def mutate_marked_ast(prefix, quoted) do
    {original_ast, nr_of_mutations_left} =
      Macro.postwalk(quoted, 0, fn ast, count ->
        replace_ast_and_count(ast, count, :left)
      end)

    {expected_ast, nr_of_mutations_right} =
      Macro.postwalk(quoted, 0, fn ast, count ->
        replace_ast_and_count(ast, count, :right)
      end)

    # debug_ast(quoted, "Marked AST")
    # debug_ast(original_ast, "Original AST")
    # debug_ast(expected_ast, "Expected AST")

    assert nr_of_mutations_left == nr_of_mutations_right, "Ooops!"
    nr_of_mutations = nr_of_mutations_left
    assert nr_of_mutations == 1, "marked AST fragments should contains exactly 1 mutation."

    m_original = Module.concat(prefix, Original)
    m_expected = Module.concat(prefix, Expected)
    m_mutated = Module.concat(prefix, Mutated)

    compile_elixir(original_ast, module: m_original)
    compile_elixir(expected_ast, module: m_expected)
    mutate_and_compile_elixir(original_ast, module: m_mutated)

    %{
      original: m_original,
      expected: m_expected,
      mutated: m_mutated
    }
  end

  defmacro left <~> right do
    expand_assert_equivalent(left, right)
  end

  defmacro assert_equivalent(left, right) do
    expand_assert_equivalent(left, right)
  end

  defp expand_assert_equivalent(left, right) do
    quote do
      lhs_tuple =
        try do
          {:ok, unquote(left)}
        rescue
          exception ->
            {:error, exception}
        end

      rhs_tuple =
        try do
          {:ok, unquote(right)}
        rescue
          exception ->
            {:error, exception}
        end

      Darwin.TestHelpers.do_assert_equivalent(
        lhs_tuple,
        rhs_tuple
      )
    end
  end

  defp maybe_append_to_message(message, mutation) do
    if ActiveMutation.default_mutation?(mutation) do
      message
    else
      message <>
        """

        Darwin.ActiveMutation.get() == #{inspect(mutation)}
        """
    end
  end

  @doc false
  def do_assert_equivalent(lhs_tuple, rhs_tuple) do
    active_mutation = ActiveMutation.pdict_aware_get()

    # Do everything inline instead of with a function
    # to avoid adding a new stack frame
    case {lhs_tuple, rhs_tuple} do
      {{:ok, lhs}, {:ok, rhs}} ->
        ExUnit.Assertions.assert(lhs == rhs)

      {{:ok, _lhs}, {:error, rhs}} ->
        if Map.get(rhs, :message) do
          new_message =
            maybe_append_to_message(
              "right hand side: " <> rhs.message,
              active_mutation
            )

          new_exception = %{rhs | message: new_message}

          raise new_exception
        else
          new_message =
            maybe_append_to_message(
              "right hand side",
              active_mutation
            )

          raise Map.put(rhs, :message, new_message)
        end

      {{:error, lhs}, {:ok, _rhs}} ->
        if Map.get(lhs, :message) do
          new_message =
            maybe_append_to_message(
              "left hand side: " <> lhs.message,
              active_mutation
            )

          new_exception = %{lhs | message: new_message}

          raise new_exception
        else
          new_message =
            maybe_append_to_message(
              "left hand side",
              active_mutation
            )

          raise Map.put(lhs, :message, new_message)
        end

      {{:error, lhs}, {:error, rhs}} ->
        %lhs_name{} = lhs
        %rhs_name{} = rhs

        message =
          maybe_append_to_message(
            """
            Both sides raised different exceptions:
            #{Exception.format(:error, lhs)} (left hand side)
            #{Exception.format(:error, rhs)} (right hand side)\
            """,
            active_mutation
          )

        ExUnit.Assertions.assert(
          lhs_name == rhs_name,
          message
        )
    end

    :ok
  end

  defp validate_codon_options!(options) do
    unless Keyword.keyword?(options) do
      raise ArgumentError, "Darwin.codon!/1 - options must be a keyword list"
    end

    for key <- [:codon, :original, :mutations] do
      unless Keyword.has_key?(options, key) do
        stringified =
          options
          |> Macro.to_string()
          |> Code.format_string!()

        raise ArgumentError, """
        Darwin.codon!/1 - key #{inspect(key)} not found in #{stringified}

            Remember you should never use this function outside a mutation template.
        """
      end
    end

    codon = Keyword.fetch!(options, :codon)
    original = Keyword.fetch!(options, :original)
    mutations = Keyword.fetch!(options, :mutations)

    {codon, original, mutations}
  end

  def gather_and_strip_mutations_ast_node(
        {
          {:., _meta1,
           [
             {:__aliases__, _meta2, [:Darwin]},
             :codon!
           ]},
          _meta3,
          [options]
        },
        mutations_list
      ) do
    {codon, original, mutations} = validate_codon_options!(options)

    new_mutations =
      for {_mutation, index} <- Enum.with_index(mutations) do
        {codon, index}
      end

    {original, new_mutations ++ mutations_list}
  end

  def gather_and_strip_mutations_ast_node(other, mutations) do
    {other, mutations}
  end

  def apply_mutation_to_ast_node(
        {
          {:., _meta1,
           [
             {:__aliases__, _meta2, [:Darwin]},
             :codon!
           ]},
          _meta3,
          [options]
        },
        {active_codon_index, active_mutation_index}
      ) do
    {codon, original, mutations} = validate_codon_options!(options)

    case codon == active_codon_index do
      # If this is the active codon we just return the original node.
      false ->
        original

      # If this is the active mutation we'll return the mutated ast
      true ->
        Enum.at(mutations, active_mutation_index)
    end
  end

  def apply_mutation_to_ast_node(other, _), do: other

  def apply_mutation(marked_ast, active_pair) do
    {ast_with_mutation, _acc} =
      Macro.postwalk(marked_ast, nil, fn ast_node, _acc ->
        applied = apply_mutation_to_ast_node(ast_node, active_pair)
        {applied, nil}
      end)

    ast_with_mutation
  end

  def process_marked_ast(prefix, marked_ast, wild_type_ast, options \\ []) do
    mutators = Keyword.get(options, :mutators, Default.mutators())

    {ast_without_mutations, reversed_mutations} =
      Macro.postwalk(marked_ast, [], &gather_and_strip_mutations_ast_node/2)

    mutations = Enum.reverse(reversed_mutations)

    code_without_mutations =
      ast_without_mutations
      |> Macro.to_string()
      |> Code.format_string!()
      |> to_string()

    wild_type_code =
      wild_type_ast
      |> Macro.to_string()
      |> Code.format_string!()
      |> to_string()

    File.write!("example1.exs", code_without_mutations)
    File.write!("example2.exs", wild_type_code)

    true = code_without_mutations == wild_type_code

    # Compile the original and mutated modules
    module_mutated = Module.concat([prefix, Mutated])
    module_original = Module.concat([prefix, Original])

    compile_elixir(wild_type_ast, module: module_original)
    mutate_and_compile_elixir(wild_type_ast, module: module_mutated, mutators: mutators)

    # Compile the expected modules
    mutation_data =
      for {codon, mutation_index} <- mutations do
        mutated_ast = apply_mutation(marked_ast, {codon, mutation_index})
        suffix = "C#{codon}M#{mutation_index}"
        module_expected = Module.concat([prefix, Expected, suffix])
        compile_elixir(mutated_ast, module: module_expected)
        # A mutation 3-tuple that can be used with the `ActiveMutation` functions:
        mutation_tuple = {module_mutated, codon, mutation_index}
        # The correspondence between the expected module name and the equivalent mutation
        {module_expected, mutation_tuple}
      end

    {module_original, module_mutated, mutation_data}
  end
end
