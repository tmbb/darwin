defmodule Darwin.TestRunner.TestRunnerLogger do
  @moduledoc false
  alias Darwin.Mutator.Mutant

  # Succint reporting

  def log_mutant(%Mutant{} = mutant, human_time) do
    case mutant.state do
      :killed -> log_mutant_killed_succint(mutant, human_time)
      :survived -> log_mutant_survived_succint(mutant, human_time)
    end
  end

  defp log_mutant_killed_succint(%Mutant{} = mutant, human_time) do
    mutation = mutant.mutation
    module = mutation.module
    codon = mutation.original_codon_index
    mut = mutation.index

    """
    Mutant killed; \
    module: #{inspect(module)}, \
    codon: #{codon}, \
    mutation: #{mut} \
    (#{human_time})\
    """
    |> in_green()
    |> IO.puts()
  end

  defp log_mutant_survived_succint(%Mutant{} = mutant, human_time) do
    mutation = mutant.mutation
    module = mutation.module
    codon = mutation.original_codon_index
    mut = mutation.index

    """
    Mutant survived; \
    module: #{inspect(module)}, \
    codon: #{codon}, \
    mutation: #{mut} \
    (#{human_time})\
    """
    |> in_red()
    |> IO.puts()
  end

  # Terminal coloring helpers
  defp in_green(text) do
    IO.ANSI.green() <> text <> IO.ANSI.reset()
  end

  defp in_red(text) do
    IO.ANSI.red() <> text <> IO.ANSI.reset()
  end
end
