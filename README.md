# Darwin

Mutation testing for Elixir.

## Installation

The package is [available in Hex](https://hex.pm/docs/publish).
It can be installed by adding `darwin` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:darwin, "~> 0.1.0"}
  ]
end
```

Documentation is on [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm).
The docs can be found at [https://hexdocs.pm/darwin](https://hexdocs.pm/darwin).

## Using Darwin

Darwin requires few changes to your workflow.
The only required changes are:

* Add the list of modules to mutate to your mix project
* Only run `ExUnit.start()` in your `test/test_helpers.exs` file if Darwin isn't running
* Add `use Darwin` to all your modules containing test cases

### Listing the modules to mutate

You can add modules for Darwin to mutate to the `:darwin` key in your mix project.
the `:modules` key expects a list of pairs in which the first element is a module name
and the second element is a keyword list containing options and their values
(by default the empty list).

```elixir
defmodule MyApp.MixProject do
  use Mix.Project

  def project do
    [
      # ...
      darwin: [
        # This will be a list of modules and their respective options.
        # Currently, no options are recognized, but the format is kept
        # to accomodate future changes
        # (for example, different test timeouts per module)
        modules: [
          {ModuleToMutate, []},
          {AnotherModuleToMutate, []}
        ]
      ]
    ]
  end
  # ...
end
```

### Running `ExUnit.start()` in your `test/test_helpers.es` only if Darwin isn't running

The function call `ExUnit.start()` does much more than just starting the `:ex_unit` app, which can be a problem for tools like Darwin, which want to take care of the way the tests are run.
In that case, `ExUnit.start()` only gets in the way, and things can go much smoother if Darwin starts ExUnit by itself.

You should change this line in your `tests/test_helper.exs`:

```elixir
ExUnit.start()
```

Into the following:

```elixir
unless Darwin.running?() do
  ExUnit.start()
end
```

That way, if you want to run "normal" tests using `mix test`, the `:ex_unit` application will be started normally.
If you want to run the mutation tests, then Darwin will start the `:ex_unit` application iself.

### Add `use Darwin` to all your modules containing test cases

Add the `use Darwin.TestCase` directive in all your modules containing test cases.
That way, the module

```elixir
defmodule MyTest do
  use ExUnit.Case, async: true
  # ...
end
```

Becomes

```elixir
defmodule MyTest do
  use ExUnit.Case, async: true
  use Darwin.TestCase
  # ...
end
```

If you don't add the `use Darwin.TestCase`, the tests will work normally, but you lose the ability to stop the test suite as soon as a single test fails. Because you'll be running the test suite hundreds of times (once per mutation), this can save an enormous amount of time. This is even more important if mutations in your code cause it to enter infinite loops, which are only recognized as failing tests after a timeout.

The `use Darwin.TestCase` directive actually overwrites the `ExUnit.Case.test/2` and `ExUnit.Case.test/3` macros, so that they won't execute after the first test fails. The test setup will be run, though (and some test setups can be quite lengthy...). The solution is to somehow add support for skipping all tests (and test setups) after the first failure in the entire suite.

## Output

Darwin will log to the console mutants that survive (in red) or are killed (in green).
The goal for the test suite is to kill as many mutants as possible.

The main output of darwin comes from the HTML reporter, though.
The reporter outputs its files into the `darwin/reports/html/` directory.
The reporter output is still under heavy development.