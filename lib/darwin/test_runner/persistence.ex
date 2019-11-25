defmodule Darwin.TestRunner.Persistence do
  alias Darwin.TestRunner.Persistence.Mutant
  alias Darwin.Mutator.Mutation

  def setup() do
    nodes = [node()]

    # Create the schema
    Memento.stop()
    Memento.Schema.create(nodes)
    Memento.start()
    Memento.Table.create!(Mutant)
  end

  def maybe_clean_module() do
    nil
  end

  def save(mutation, killed) do
    mutant = %Mutant{
      mutation_id: Mutation.to_tuple(mutation),
      mutation: mutation,
      module: mutation.module,
      line: mutation.line,
      killed: killed
    }

    Memento.transaction!(fn ->
      Memento.Query.write(mutant)
    end)
  end

  def all_mutants() do
    Memento.transaction!(fn ->
      Memento.Query.all(Mutant)
    end)
  end
end
