defmodule TimeConvert do
  @minute 60
  @hour @minute * 60
  @day @hour * 24
  @week @day * 7
  @divisor [@week, @day, @hour, @minute, 1]

  defp sec_to_str(sec) do
    {_, [s, m, h, d, w]} =
      Enum.reduce(@divisor, {sec, []}, fn divisor, {n, acc} ->
        {rem(n, divisor), [n / divisor | acc]}
      end)

    [
      "#{trunc(w)}wk",
      "#{trunc(d)}d",
      "#{trunc(h)}h",
      "#{trunc(m)}m",
      "#{Float.round(s, 3)}s"
    ]
    |> Enum.reject(fn str ->
      str == "0.0" or (String.starts_with?(str, "0") and not String.starts_with?(str, "0."))
    end)
    |> Enum.join(", ")
  end

  def microsec_to_str(ms), do: trunc(ms / 1_000_000) |> sec_to_str()
end

defmodule Darwin.TestRunner do
  alias Darwin.Mutator
  alias Darwin.Beam
  alias Darwin.Mutator.Context
  alias Darwin.ActiveMutation
  require Logger

  def mutate_compile_and_load_module(module_name) do
    {mutated_form_list, ctx} = Mutator.mutate_module(module_name)
    contents = Darwin.Erlang.pprint_forms(mutated_form_list)
    File.write!("mutated_dump.erl", contents)
    Beam.compile_and_load(mutated_form_list)
    ctx
  end

  def mutate_modules(module_names) do
    Task.async_stream(module_names, fn module_name ->
      mutate_compile_and_load_module(module_name)
    end)
    |> Enum.to_list()
    |> Enum.map(fn {:ok, result} -> result end)
  end

  defp mutation_to_tuple(mutation) do
    {mutation.module, mutation.original_codon_index, mutation.index}
  end

  require Logger

  @config [
    formatters: [],
    autorun: false,
    max_failures: 1,
    timeout: 3000
  ]

  def reset_ex_unit_server(modules) do
    %{async_modules: async, sync_modules: sync} = modules

    for module <- async do
      ExUnit.Server.add_async_module(module)
    end

    for module <- sync do
      ExUnit.Server.add_sync_module(module)
    end

    :ok
  end

  defp get_test_files() do
    Path.wildcard("test/**/*_test.exs")
  end

  defp add_modules_to_ex_unit_server(test_files) do
    Kernel.ParallelCompiler.compile(test_files)
  end

  def get_modules_from_ex_unit_server() do
    %{async_modules: async, sync_modules: sync} = :sys.get_state(ExUnit.Server)
    # Ignore the other keys in the map:
    %{async_modules: async, sync_modules: sync}
  end

  def run_test_suite(test_files, config) do
    Mix.Compilers.Test.require_and_run(test_files, ["test"], config)
  end

  def create_and_hunt_mutants(module_names) do
    ex_unit_config =
      ExUnit.configuration()
      |> Keyword.merge(Application.get_all_env(:ex_unit))

    darwin_config =
      ExUnit.configuration()
      |> Keyword.merge(Application.get_all_env(:ex_unit))
      |> Keyword.merge(@config)

    test_files = get_test_files()
    add_modules_to_ex_unit_server(test_files)
    modules = get_modules_from_ex_unit_server()

    # ...
    ExUnit.start()

    # Mutate the modules
    _test_suite = run_test_suite(test_files, ex_unit_config)

    module_contexts = mutate_modules(module_names)

    ExUnit.configure(darwin_config)

    {darwin_delta, _value} =
      :timer.tc(fn ->
        for ctx <- module_contexts do
          for mutation <- ctx.mutations do
            # Activate the new mutation
            ActiveMutation.put(mutation)
            # Reset the server so that we can test our modules again
            reset_ex_unit_server(modules)
            # Run the tests (without having to recompile the test modules!)
            {mutation_delta, {:ok, test_suite}} =
              :timer.tc(fn ->
                run_test_suite(test_files, darwin_config)
              end)

            # Humanize the time delta
            mutation_human_time = TimeConvert.microsec_to_str(mutation_delta)
            original_codon = Context.original_codon(ctx, mutation)

            test_suite =
              if test_suite[:failures] > 0 do
                log_mutant_killed(mutation, original_codon, mutation_human_time)
              else
                log_mutant_survived(mutation, original_codon, mutation_human_time)
              end

            {mutation, test_suite}
          end
        end
      end)

    human_time = TimeConvert.microsec_to_str(darwin_delta)
    Logger.info("Total time: #{human_time}")

    :ok
  end

  defp in_green(text) do
    IO.ANSI.green() <> text <> IO.ANSI.reset()
  end

  defp in_red(text) do
    IO.ANSI.red() <> text <> IO.ANSI.reset()
  end

  defp log_mutant_killed(mutation, original_codon, human_time) do
    tuple = mutation_to_tuple(mutation)
    mutation_name = mutation.name
    original_code = Macro.to_string(original_codon.value.elixir)
    mutated_code = Macro.to_string(mutation.mutated_codon.elixir)
    line = original_codon.line

    IO.puts("""
    #{in_green("Mutant killed: #{inspect(tuple)} (#{human_time})")}
    - Line: #{line}
    - Mutation name: #{mutation_name}
    - Original code: #{original_code}
    - Mutated code: #{mutated_code}
    """)
  end

  defp log_mutant_survived(mutation, original_codon, human_time) do
    tuple = mutation_to_tuple(mutation)
    mutation_name = mutation.name
    original_code = Macro.to_string(original_codon.value.elixir)
    mutated_code = Macro.to_string(mutation.mutated_codon.elixir)
    line = original_codon.line

    IO.puts("""
    #{in_red("Mutant survived: #{inspect(tuple)} (#{human_time})")}
    - Line: #{line}
    - Mutation name: #{mutation_name}
    - Original code: #{original_code}
    - Mutated code: #{mutated_code}
    """)
  end
end
