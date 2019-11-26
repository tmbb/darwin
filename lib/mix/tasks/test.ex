defmodule Mix.Tasks.Darwin.Test do
  use Mix.Task

  @shortdoc "Runs mutation testing"
  def run(_args) do
    unless System.get_env("MIX_ENV") || Mix.env() == :test do
      Mix.raise(
        "\"mix test\" is running in the \"#{Mix.env()}\" environment. If you are " <>
          "running tests alongside another task, please set MIX_ENV explicitly"
      )
    end

    Mix.shell().print_app
    Mix.Task.run("app.start", [])

    # Start the ExUnit application as a "normal" application.
    # Don't use the `ExUnit.start()` function because that way ExUnit
    # tries to control how the tessts are run.
    ExUnit.start([], [])

    case Application.load(:ex_unit) do
      :ok -> :ok
      {:error, {:already_loaded, :ex_unit}} -> :ok
    end

    Application.put_env(:darwin, :darwin_is_running?, true)

    modules = get_modules()
    require_test_helper()
    Darwin.TestRunner.create_and_hunt_mutants(modules)

    exit({:shutdown, 0})
  end

  defp get_modules() do
    config = Mix.Project.config()
    darwin = Keyword.get(config, :darwin, [])
    modules = Keyword.get(darwin, :modules, [])
    modules
  end

  defp require_test_helper do
    file = Path.join("test", "test_helper.exs")

    if File.exists?(file) do
      Code.require_file(file)
    else
      Mix.raise("Cannot run tests because test helper file #{inspect(file)} does not exist")
    end
  end
end
