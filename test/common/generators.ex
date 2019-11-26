defmodule DarwinTest.Generators do
  @doc """
  StreamData generators meant to be used when testing mutations.
  """

  @min_mutation 0
  @max_mutation 999

  @min_codon 0
  @max_codon 999

  @modules Enum.map('ABCDEFGHIJKLMNOPQRSTUV', fn c ->
             Module.concat(Module, <<c>>)
           end)

  # Generates atoms at runtime!!!
  def module(opts \\ []) do
    prefix = Keyword.get(opts, :prefix)

    modules =
      if prefix do
        Enum.map(@modules, fn suffix -> Module.concat(prefix, suffix) end)
      else
        @modules
      end

    modules
    |> Enum.map(&StreamData.constant/1)
    |> StreamData.one_of()
  end

  def codon() do
    StreamData.integer(@min_codon..@max_codon)
  end

  def mutation(opts \\ []) do
    min = Keyword.get(opts, :min, @min_mutation)
    max = Keyword.get(opts, :max, @max_mutation)
    StreamData.integer(min..max)
  end
end
