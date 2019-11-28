defmodule Darwin.TestRunner.TestRunnerArguments do
  @moduledoc false
  defstruct test_helper_path: "tests/test_helper.ex",
            test_file_patterns: ["test/**/*_test.exs"],
            modules_to_mutate: [],
            options: []

  def new(opts) do
    struct(__MODULE__, opts)
  end
end
