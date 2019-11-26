defmodule Darwin.TestRunner do
  @moduledoc false
  alias Darwin.Mutator
  alias Darwin.Beam
  alias Darwin.Mutator.Context
  alias Darwin.Mutator.Mutation
  alias Darwin.ActiveMutation
  alias Darwin.MutationServer
  alias Darwin.Utils.TimeConvert
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

  require Logger

  @config [
    formatters: [Darwin.TestRunner.Formatter],
    autorun: false,
    max_failures: 1,
    timeout: 3000
  ]

  defp reset_ex_unit_server(modules) do
    # BLACK MAGIC: This uses private APIs
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
    # The way to add modules to the ExUnit server is to compile the test suite
    Kernel.ParallelCompiler.compile(test_files)
  end

  defp get_modules_from_ex_unit_server() do
    # BLACK MAGIC:
    # - Not only does this use private APIs, it directly inspects the state of a (private) GenServer!
    %{async_modules: async, sync_modules: sync} = :sys.get_state(ExUnit.Server)
    # Ignore the other keys in the map:
    %{async_modules: async, sync_modules: sync}
  end

  defp run_test_suite() do
    # Do I really have to call `ExUnit.Server.modules_loaded/0`?
    ExUnit.Server.modules_loaded()
    ExUnit.run()
  end

  def create_and_hunt_mutants(modules_to_mutate) do
    # Suppress the warnigs we get when we recompile the test cases in memory
    Code.compiler_options(ignore_module_conflict: true)

    ex_unit_config =
      ExUnit.configuration()
      |> Keyword.merge(Application.get_all_env(:ex_unit))

    darwin_config =
      ExUnit.configuration()
      |> Keyword.merge(Application.get_all_env(:ex_unit))
      |> Keyword.merge(@config)

    # Store the test files; we'll need them to add the modules into the `ExUnit.Server`
    test_files = get_test_files()
    # Add the test case modules to the `ExUnit.Server` so that they will be tested
    add_modules_to_ex_unit_server(test_files)
    # Store the test case modules from `ExUnit.Server` so that they can be reused
    test_case_modules = get_modules_from_ex_unit_server()
    # Configure `ExUnit` for a "normal" test run with unmutated code
    ExUnit.configure(ex_unit_config)
    # No need to populate the `ExUnit.Server` when we run the test suite for the first time;
    # The `ExUnit.Server` is already populated
    unmutated_test_suite = run_test_suite()
    %{failures: failures} = unmutated_test_suite

    if failures > 0 do
      Logger.error(
        "Some tests in the original test suite failed. " <>
          "Darwin won't run mutation tests until all tests pass."
      )
    else
      mutate_and_test(modules_to_mutate, test_case_modules, darwin_config)
    end
  end

  defp mutate_and_test(modules_to_mutate, test_case_modules, darwin_config) do
    module_names_to_mutate = for {module_name, _opts} <- modules_to_mutate, do: module_name
    module_contexts = mutate_modules(module_names_to_mutate)
    ExUnit.configure(darwin_config)

    {darwin_delta, _value} =
      :timer.tc(fn ->
        for ctx <- module_contexts do
          for mutation <- ctx.mutations do
            # Prepare the mutation server for a new suite
            MutationServer.start_suite()
            # Activate the new mutation
            ActiveMutation.put(mutation)
            # Reset the server so that we can test our modules again
            reset_ex_unit_server(test_case_modules)
            ExUnit.configure(darwin_config)
            # Run the tests (without having to recompile the test modules!)
            {mutation_delta, test_suite} =
              :timer.tc(fn ->
                run_test_suite()
              end)

            # Humanize the time delta
            mutation_human_time = TimeConvert.microsec_to_str(mutation_delta)
            original_codon = Context.original_codon(ctx, mutation)

            if test_suite[:failures] > 0 do
              log_mutant_killed(ctx, mutation, original_codon, mutation_human_time)
            else
              log_mutant_survived(ctx, mutation, original_codon, mutation_human_time)
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

  defp log_mutant_killed(_ctx, mutation, original_codon, human_time) do
    tuple = Mutation.to_tuple(mutation)
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

  @radius 3

  require EEx

  defp log_mutant_survived(ctx, mutation, original_codon, human_time) do
    tuple = Mutation.to_tuple(mutation)
    mutation_name = mutation.name
    original_code = Macro.to_string(original_codon.value.elixir)
    mutated_code = Macro.to_string(mutation.mutated_codon.elixir)
    line = original_codon.line

    line_min = line - @radius
    line_max = line + @radius

    line_range = line_min..line_max

    lines =
      ctx.source_path
      |> File.read!()
      |> String.split("\n")
      |> Enum.slice(line_range)
      |> Enum.with_index(line_min)

    code =
      EEx.eval_string(
        """
        <%= for {line, nr} <- lines do %>
            <%= nr %> <%= line %><% end %>
        """,
        lines: lines
      )

    IO.puts("""
    #{in_red("Mutant survived: #{inspect(tuple)} (#{human_time})")}
    - Line: #{line}
    - Mutation name: #{mutation_name}
    - Original code: #{original_code}
    - Mutated code: #{mutated_code}
    #{code}
    """)
  end
end
