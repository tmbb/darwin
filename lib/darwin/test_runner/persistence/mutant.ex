defmodule Darwin.TestRunner.Persistence.Mutant do
  use Memento.Table,
    attributes: [:id, :killed, :mutation_id, :module, :line, :mutation],
    index: [:killed],
    type: :ordered_set,
    autoincrement: true

  def group_by_module_and_line(mutants) do
    mutants
    |> group_by_module()
    |> Enum.map(&group_by_line/1)
  end

  defp group_by_module(mutants) do
    Enum.group_by(mutants, fn mutant -> mutant.module end)
  end

  defp group_by_line(mutants) do
    Enum.group_by(mutants, fn mutant -> mutant.line end)
  end
end
