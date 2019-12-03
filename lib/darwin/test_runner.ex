defmodule Darwin.TestRunner do
  @moduledoc false
  require Logger
  alias Darwin.Mutator
  alias Darwin.Mutator.Context
  alias Darwin.Mutator.Mutant
  alias Darwin.ActiveMutation
  alias Darwin.MutationServer
  alias Darwin.Utils.TimeConvert
  alias Darwin.Utils.SafeSourcePath
  alias Darwin.Reporters.HtmlReporter

  alias Darwin.TestRunner.{
    TestRunnerArguments,
    TestRunnerLogger
  }

  @config [
    formatters: [Darwin.TestRunner.Formatter],
    autorun: false,
    max_failures: 1,
    timeout: 3000
  ]

  def create_and_hunt_mutants(arguments) do
    # Everything in this function is parametrized by the `arguments`.
    # This makes it easier to test the `mix darwin.test` task,
    # because you can just run this function with different arguments.
    %{modules_to_mutate: modules_to_mutate} = arguments

    source_paths =
      for {module, _opts} <- modules_to_mutate, into: %{} do
        {module, SafeSourcePath.source_path_for_module(module)}
      end

    # Start the ExUnit application as a "normal" application.
    # Don't use the `ExUnit.start()` function because that way ExUnit
    # tries to control how the tests are run.
    ExUnit.start([], [])

    case Application.load(:ex_unit) do
      :ok -> :ok
      {:error, {:already_loaded, :ex_unit}} -> :ok
    end

    # Before requiring the test helper, make sure the runtime "knows" that Darwin is running.
    # The test helper might contain code meant to be running ony if darwin is running.
    Darwin.darwin_is_now_running()

    # Require the test helper file, which conatins setup code necessary to run the tests.
    require_test_helper(arguments)

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
    test_files = get_test_files(arguments)
    # Add the test case modules to the `ExUnit.Server` so that they will be tested
    add_modules_to_ex_unit_server(test_files)
    # Store the test case modules from `ExUnit.Server` so that they can be reused
    test_case_modules = get_modules_from_ex_unit_server()
    # Configure `ExUnit` for a "normal" test run with unmutated code
    ExUnit.configure(ex_unit_config)
    # No need to populate the `ExUnit.Server` when we run the test suite for the first time;
    # The `ExUnit.Server` is already populated
    unmutated_test_suite_results = run_test_suite()
    %{failures: failures} = unmutated_test_suite_results

    if failures > 0 do
      Mix.raise(
        "Some tests in the original test suite failed. " <>
          "Darwin won't run mutation tests until all tests pass."
      )
    end

    # Then we must mutate the modules and tun the test suite without activating any mutation.
    reset_ex_unit_server(test_case_modules)
    module_contexts = mutate_modules(modules_to_mutate)
    mutated_test_suite_results = run_test_suite()
    # After mutating the modules, we must ensure that the mutated and unmutated and unmutated
    # test suites return the same result.
    # Because we've already tested that all tests pass, this only tests whether
    # the tests pass in both the mutated and unmutated test suites.
    # This is a very crude sanity check to make sure or mutations aren't changing the code semantics.
    ensure_test_suites_are_equivalent!(unmutated_test_suite_results, mutated_test_suite_results)
    # We can finaly test the mutated modules:
    test_mutated_modules(source_paths, module_contexts, test_case_modules, darwin_config)

    Darwin.darwin_has_stopped()
  end

  def ensure_test_suites_are_equivalent!(unmutated, mutated) do
    if unmutated != mutated do
      Darwin.Exceptions.raise("""
      The mutated and the original test suites are not equivalent.
      This is probably a bug in one of the mutators.
      If you are using Darwin's default mutators, please report a bug.

      - Original test suite results: #{inspect(unmutated)}
      - Mutated test suite results: #{inspect(mutated)}
      """)
    end
  end

  def mutate_modules(modules_to_mutate) do
    module_names_to_mutate = for {module_name, _opts} <- modules_to_mutate, do: module_name

    Task.async_stream(module_names_to_mutate, fn module_name ->
      Mutator.mutate_compile_and_load_module(module_name)
    end)
    |> Enum.to_list()
    |> Enum.map(fn {:ok, result} -> result end)
  end

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

  defp get_test_files(%TestRunnerArguments{} = arguments) do
    %{test_file_patterns: patterns} = arguments
    Enum.flat_map(patterns, &Path.wildcard/1)
  end

  defp require_test_helper(%TestRunnerArguments{} = arguments) do
    %{test_helper_path: test_helper_path} = arguments

    if File.exists?(test_helper_path) do
      Code.require_file(test_helper_path)
    else
      Darwin.Exceptions.raise(
        "Cannot run tests because test helper file #{inspect(test_helper_path)} does not exist"
      )
    end
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

  defp test_mutated_modules(code_paths, module_contexts, test_case_modules, darwin_config) do
    ExUnit.configure(darwin_config)

    {darwin_delta, mutants} =
      :timer.tc(fn ->
        for ctx <- Enum.reverse(module_contexts) do
          for mutation <- Enum.reverse(ctx.mutations) do
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

            state =
              if test_suite[:failures] > 0 do
                :killed
              else
                :survived
              end

            mutant = Mutant.new(mutation: mutation, state: state, original_codon: original_codon)
            TestRunnerLogger.log_mutant(mutant, mutation_human_time)

            mutant
          end
        end
      end)

    grouped_mutants =
      mutants
      |> List.flatten()
      |> Mutant.group_by_module()

    HtmlReporter.run(code_paths, grouped_mutants)

    human_time = TimeConvert.microsec_to_str(darwin_delta)
    Logger.info("Total time: #{human_time}")

    :ok
  end
end
