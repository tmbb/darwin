defmodule Darwin.DefaultMutators.RuntimeTests.CaseTest do
  use ExUnit.Case, async: true
  import Darwin.TestHelpers
  alias Darwin.ActiveMutation
  require ActiveMutation

  @invalid_mutation {nil, nil, nil}

  test "example #1 (all mutators)" do
    marked =
      quote do
        def f(x) do
          case x do
            {Darwin.codon!(codon: 0, original: :ok, mutations: [:darwin_was_here]), value} ->
              case value do
                Darwin.codon!(codon: 1, original: 1, mutations: [9]) ->
                  Darwin.codon!(codon: 2, original: :one, mutations: [:darwin_was_here])

                Darwin.codon!(codon: 3, original: 2, mutations: [0]) ->
                  Darwin.codon!(codon: 4, original: :two, mutations: [:darwin_was_here])
              end

            Darwin.codon!(codon: 5, original: :error, mutations: [:darwin_was_here]) ->
              Darwin.codon!(codon: 6, original: :error, mutations: [:darwin_was_here])
          end
        end
      end

    unmarked =
      quote do
        def f(x) do
          case x do
            {:ok, value} ->
              case value do
                1 -> :one
                2 -> :two
              end

            :error ->
              :error
          end
        end
      end

    {m_original, m_mutated, expected} = process_marked_ast(__MODULE__.Example1, marked, unmarked)

    # No mutations
    assert m_original.f({:ok, 1}) == :one
    assert m_original.f({:ok, 2}) == :two
    assert m_original.f(:error) == :error
    assert_raise CaseClauseError, fn -> m_original.f(:invalid) end
    assert_raise CaseClauseError, fn -> m_original.f({:ok, :invalid}) end

    # Run equivalences with a dummy mutation
    ActiveMutation.with_mutation(@invalid_mutation, fn ->
      m_original.f({:ok, 1}) <~> m_mutated.f({:ok, 1})
      m_original.f({:ok, 1}) <~> m_mutated.f({:ok, 1})
      m_original.f({:ok, 2}) <~> m_mutated.f({:ok, 2})
      m_original.f(:error) <~> m_mutated.f(:error)
      m_original.f(:invalid) <~> m_mutated.f(:invalid)
      m_original.f({:ok, :invalid}) <~> m_mutated.f({:ok, :invalid})
    end)

    for {m_expected, mutation} <- expected do
      ActiveMutation.with_mutation(mutation, fn ->
        m_expected.f({:ok, 1}) <~> m_mutated.f({:ok, 1})
        m_expected.f({:ok, 1}) <~> m_mutated.f({:ok, 1})
        m_expected.f({:ok, 2}) <~> m_mutated.f({:ok, 2})
        m_expected.f(:error) <~> m_mutated.f(:error)
        m_expected.f(:invalid) <~> m_mutated.f(:invalid)
        m_expected.f({:ok, :invalid}) <~> m_mutated.f({:ok, :invalid})
      end)
    end
  end
end
