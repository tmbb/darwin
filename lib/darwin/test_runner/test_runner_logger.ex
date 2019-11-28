defmodule Darwin.TestRunner.TestRunnerLogger do
  @moduledoc false
  require EEx

  alias Darwin.Mutator.{
    Mutation,
    Codon
  }

  @radius 3

  defp in_green(text) do
    IO.ANSI.green() <> text <> IO.ANSI.reset()
  end

  defp in_red(text) do
    IO.ANSI.red() <> text <> IO.ANSI.reset()
  end

  def log_mutant_killed(_ctx, %Mutation{} = mutation, %Codon{} = original_codon, human_time) do
    {module, codon, mut} = Mutation.to_tuple(mutation)
    mutation_name = mutation.name
    original_code = Macro.to_string(original_codon.value.elixir)
    mutated_code = Macro.to_string(mutation.mutated_codon.elixir)
    line = original_codon.line

    IO.puts("""
    #{
      in_green(
        "Mutant killed; module: #{inspect(module)}, codon: #{codon}, mutation: #{mut} (#{
          human_time
        })"
      )
    }
    - Line: #{line}
    - Mutation name: #{mutation_name}
    - Original code: #{original_code}
    - Mutated code: #{mutated_code}
    """)
  end

  def log_mutant_survived(ctx, mutation, original_codon, human_time) do
    {module, codon, mut} = Mutation.to_tuple(mutation)
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
    #{
      in_red(
        "Mutant survived; module: #{inspect(module)}, codon: #{codon}, mutation: #{mut} (#{
          human_time
        })"
      )
    }
    - Line: #{line}
    - Mutation name: #{mutation_name}
    - Original code: #{original_code}
    - Mutated code: #{mutated_code}
    #{code}\
    """)
  end
end
