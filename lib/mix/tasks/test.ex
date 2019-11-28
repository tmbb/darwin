defmodule Mix.Tasks.Darwin.Test do
  use Mix.Task
  alias Darwin.TestRunner
  alias Darwin.TestRunner.TestRunnerArguments

  @shortdoc "Runs mutation testing"

  def run(_args) do
    # The `run/1` function for this task is very simple.
    # Most of the logic has been moved into the `Darwin.TestRunner.create_and_hunt_mutants/1` function.
    # This makes it easier to write end-to-end tests, because arguments are not hardcoded
    # and we can simulate running the mix task in different projects by feeding custom arguments
    # into the `create_and_hunt_mutants/1` function.
    #
    # This will be useful when Darwin gets some proper end-to-end tests.
    unless System.get_env("MIX_ENV") || Mix.env() == :test do
      Mix.raise(
        "\"mix test\" is running in the \"#{Mix.env()}\" environment. If you are " <>
          "running tests alongside another task, please set MIX_ENV explicitly"
      )
    end

    Mix.shell().print_app
    Mix.Task.run("app.start", [])

    modules_to_mutate = get_modules()

    arguments =
      TestRunnerArguments.new(
        test_helper_path: "test/test_helper.exs",
        test_file_patterns: ["test/**/*_test.exs"],
        modules_to_mutate: modules_to_mutate,
        # TODO: Extract the options form the configuration and use them somewhere
        options: []
      )

    # Perform the actual work:
    TestRunner.create_and_hunt_mutants(arguments)

    exit({:shutdown, 0})
  end

  # Get modules from the `mix.exs` config.
  defp get_modules() do
    config = Mix.Project.config()
    darwin = Keyword.get(config, :darwin, [])
    modules = Keyword.get(darwin, :modules, [])
    modules
  end
end
