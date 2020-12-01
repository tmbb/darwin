defmodule Darwin.DefaultMutators.RuntimeTests.CaseTest do
  use ExUnit.Case, async: true
  import Darwin.TestHelpers
  alias Darwin.ActiveMutation
  require ActiveMutation

  @invalid_mutation {nil, nil, nil}

  test "example #1 - Codon 0, mutation 0" do
    marked =
      quote do
        def f(x) do
          case x do
            {MUTATION[:ok >>> :darwin_was_here], value} ->
              case value do
                1 -> :one
                2 -> :two
              end

            :error ->
              :error
          end
        end
      end

    modules = mutate_marked_ast(__MODULE__.E1C0M0, marked)

    %{original: m_original, expected: m_expected, mutated: m_mutated} = modules

    right_mutation = {m_mutated, 0, 0}

    # -------------------------
    # No mutations
    # -------------------------
    # The original module should be equivalent to the mutated modules
    # (the mutations in the mutated module are activated conditionally)
    assert m_original.f({:ok, 1}) == :one
    assert m_mutated.f({:ok, 1}) == :one

    assert m_original.f({:ok, 2}) == :two
    assert m_mutated.f({:ok, 2}) == :two

    assert m_original.f(:error) == :error
    assert m_mutated.f(:error) == :error

    assert_raise CaseClauseError, fn -> m_original.f(:invalid) end
    assert_raise CaseClauseError, fn -> m_mutated.f(:invalid) end

    assert_raise CaseClauseError, fn -> m_original.f({:ok, :invalid}) end
    assert_raise CaseClauseError, fn -> m_mutated.f({:ok, :invalid}) end

    # - The expected module should have the mutation "always active"
    assert m_expected.f({:darwin_was_here, 1}) == :one
    assert_raise CaseClauseError, fn -> m_expected.f({:ok, 1}) end
    assert_raise CaseClauseError, fn -> m_expected.f({:ok, 2}) end
    assert m_expected.f(:error) == :error
    assert_raise CaseClauseError, fn -> m_expected.f(:invalid) end
    assert_raise CaseClauseError, fn -> m_expected.f({:ok, :invalid}) end

    # ----------------------------------------------
    # Mutated module with the wrong mutation
    # ----------------------------------------------
    # Should be equivalent to the original module)

    # Repeat the `ActiveMutation.with_default_mutation/1` call
    # instead of having everything inside the same call for
    # better error reporting.
    # Unfortunately, the Darwin mutators turn normal elixir code
    # into something that's problematic to debug.
    # The exceptions that mutated code raises are the same as the ones
    # from non-mutated code, but source location will be unavailable.
    wrong_mutation = @invalid_mutation

    ActiveMutation.with_mutation(wrong_mutation, fn ->
      assert m_original.f({:ok, 1}) == :one
      assert m_mutated.f({:ok, 1}) == :one
    end)

    ActiveMutation.with_mutation(wrong_mutation, fn ->
      assert m_original.f({:ok, 1}) == :one
      assert m_mutated.f({:ok, 1}) == :one
    end)

    ActiveMutation.with_mutation(wrong_mutation, fn ->
      assert m_original.f({:ok, 2}) == :two
      assert m_mutated.f({:ok, 2}) == :two
    end)

    ActiveMutation.with_mutation(wrong_mutation, fn ->
      assert m_original.f(:error) == :error
      assert m_mutated.f(:error) == :error
    end)

    ActiveMutation.with_mutation(wrong_mutation, fn ->
      assert_raise CaseClauseError, fn -> m_original.f(:invalid) end
      assert_raise CaseClauseError, fn -> m_mutated.f(:invalid) end
    end)

    ActiveMutation.with_mutation(wrong_mutation, fn ->
      assert_raise CaseClauseError, fn -> m_original.f({:ok, :invalid}) end
      assert_raise CaseClauseError, fn -> m_mutated.f({:ok, :invalid}) end
    end)

    # ------------------------------------------------
    # Mutated module with the right mutation
    # ------------------------------------------------
    # Should be equivalent to the expected module
    ActiveMutation.with_mutation(right_mutation, fn ->
      assert m_mutated.f({:darwin_was_here, 1}) == :one
      assert m_expected.f({:darwin_was_here, 1}) == :one
    end)

    ActiveMutation.with_mutation(right_mutation, fn ->
      assert_raise CaseClauseError, fn -> m_mutated.f({:ok, 1}) end
      assert_raise CaseClauseError, fn -> m_expected.f({:ok, 1}) end
    end)

    ActiveMutation.with_mutation(right_mutation, fn ->
      assert_raise CaseClauseError, fn -> m_mutated.f({:ok, 2}) end
      assert_raise CaseClauseError, fn -> m_expected.f({:ok, 2}) end
    end)

    ActiveMutation.with_mutation(right_mutation, fn ->
      assert m_mutated.f(:error) == :error
      assert m_expected.f(:error) == :error
    end)

    ActiveMutation.with_mutation(right_mutation, fn ->
      assert_raise CaseClauseError, fn -> m_mutated.f(:invalid) end
      assert_raise CaseClauseError, fn -> m_expected.f(:invalid) end
    end)

    ActiveMutation.with_mutation(right_mutation, fn ->
      assert_raise CaseClauseError, fn -> m_mutated.f({:ok, :invalid}) end
      assert_raise CaseClauseError, fn -> m_expected.f({:ok, :invalid}) end
    end)
  end

  test "example #1 - Codon 1, mutation 0" do
    marked =
      quote do
        def f(x) do
          case x do
            {:ok, value} ->
              case value do
                MUTATION[1 >>> 0] -> :one
                2 -> :two
              end

            :error ->
              :error
          end
        end
      end

    modules = mutate_marked_ast(__MODULE__.E1C1M0, marked)

    %{original: m_original, expected: m_expected, mutated: m_mutated} = modules

    right_mutation = {m_mutated, 1, 0}

    # -------------------------
    # No mutations
    # -------------------------
    # The original module should be equivalent to the mutated modules
    # (the mutations in the mutated module are activated conditionally)
    assert m_original.f({:ok, 1}) == :one
    assert m_mutated.f({:ok, 1}) == :one

    assert m_original.f({:ok, 2}) == :two
    assert m_mutated.f({:ok, 2}) == :two

    assert m_original.f(:error) == :error
    assert m_mutated.f(:error) == :error

    assert_raise CaseClauseError, fn -> m_original.f(:invalid) end
    assert_raise CaseClauseError, fn -> m_mutated.f(:invalid) end

    assert_raise CaseClauseError, fn -> m_original.f({:ok, :invalid}) end
    assert_raise CaseClauseError, fn -> m_mutated.f({:ok, :invalid}) end

    # The expected module should have the mutation "always active"
    assert_raise CaseClauseError, fn -> m_expected.f({:ok, 1}) end
    assert m_expected.f({:ok, 2}) == :two
    assert m_expected.f(:error) == :error
    assert_raise CaseClauseError, fn -> m_expected.f(:invalid) end
    assert_raise CaseClauseError, fn -> m_expected.f({:ok, :invalid}) end

    assert m_expected.f({:ok, 0}) == :one

    # ----------------------------------------------
    # Mutated module with the wrong mutation
    # ----------------------------------------------
    # Should be equivalent to the original module)

    ActiveMutation.with_mutation(@invalid_mutation, fn ->
      m_original.f({:ok, 1}) <~> m_mutated.f({:ok, 1})
      m_original.f({:ok, 1}) <~> m_mutated.f({:ok, 1})
      m_original.f({:ok, 2}) <~> m_mutated.f({:ok, 2})
      m_original.f(:error) <~> m_mutated.f(:error)
      m_original.f(:invalid) <~> m_mutated.f(:invalid)
      m_original.f({:ok, :invalid}) <~> m_mutated.f({:ok, :invalid})
    end)

    # ------------------------------------------------
    # Mutated module with the right mutation
    # ------------------------------------------------
    # Should be equivalent to the expected module

    ActiveMutation.with_mutation(right_mutation, fn ->
      m_mutated.f({:ok, 1}) <~> m_expected.f({:ok, 1})
      m_mutated.f({:ok, 1}) <~> m_expected.f({:ok, 1})
      m_mutated.f({:ok, 2}) <~> m_expected.f({:ok, 2})
      m_mutated.f(:error) <~> m_expected.f(:error)
      m_mutated.f(:invalid) <~> m_expected.f(:invalid)
      m_mutated.f({:ok, :invalid}) <~> m_expected.f({:ok, :invalid})
    end)
  end

  test "example #1 - Codon 2, mutation 0" do
    marked =
      quote do
        def f(x) do
          case x do
            {:ok, value} ->
              case value do
                1 -> MUTATION[:one >>> :darwin_was_here]
                2 -> :two
              end

            :error ->
              :error
          end
        end
      end

    modules = mutate_marked_ast(__MODULE__.E1C2M0, marked)

    %{original: m_original, expected: m_expected, mutated: m_mutated} = modules

    right_mutation = {m_mutated, 2, 0}

    # -------------------------
    # No mutations
    # -------------------------
    # The original module should be equivalent to the mutated modules
    # (the mutations in the mutated module are activated conditionally)
    assert m_original.f({:ok, 1}) == :one
    assert m_mutated.f({:ok, 1}) == :one

    assert m_original.f({:ok, 2}) == :two
    assert m_mutated.f({:ok, 2}) == :two

    assert m_original.f(:error) == :error
    assert m_mutated.f(:error) == :error

    assert_raise CaseClauseError, fn -> m_original.f(:invalid) end
    assert_raise CaseClauseError, fn -> m_mutated.f(:invalid) end

    assert_raise CaseClauseError, fn -> m_original.f({:ok, :invalid}) end
    assert_raise CaseClauseError, fn -> m_mutated.f({:ok, :invalid}) end

    # The expected module should have the mutation "always active"
    assert m_expected.f({:ok, 1}) == :darwin_was_here
    assert m_expected.f({:ok, 2}) == :two
    assert m_expected.f(:error) == :error
    assert_raise CaseClauseError, fn -> m_expected.f(:invalid) end
    assert_raise CaseClauseError, fn -> m_expected.f({:ok, :invalid}) end

    # ----------------------------------------------
    # Mutated module with the wrong mutation
    # ----------------------------------------------
    # Should be equivalent to the original module)

    ActiveMutation.with_mutation(@invalid_mutation, fn ->
      m_original.f({:ok, 1}) <~> m_mutated.f({:ok, 1})
      m_original.f({:ok, 1}) <~> m_mutated.f({:ok, 1})
      m_original.f({:ok, 2}) <~> m_mutated.f({:ok, 2})
      m_original.f(:error) <~> m_mutated.f(:error)
      m_original.f(:invalid) <~> m_mutated.f(:invalid)
      m_original.f({:ok, :invalid}) <~> m_mutated.f({:ok, :invalid})
    end)

    # ------------------------------------------------
    # Mutated module with the right mutation
    # ------------------------------------------------
    # Should be equivalent to the expected module

    ActiveMutation.with_mutation(right_mutation, fn ->
      m_mutated.f({:ok, 1}) <~> m_expected.f({:ok, 1})
      m_mutated.f({:ok, 1}) <~> m_expected.f({:ok, 1})
      m_mutated.f({:ok, 2}) <~> m_expected.f({:ok, 2})
      m_mutated.f(:error) <~> m_expected.f(:error)
      m_mutated.f(:invalid) <~> m_expected.f(:invalid)
      m_mutated.f({:ok, :invalid}) <~> m_expected.f({:ok, :invalid})
    end)
  end

  test "example #1 - Codon 3, mutation 0" do
    marked =
      quote do
        def f(x) do
          case x do
            {:ok, value} ->
              case value do
                1 -> :one
                MUTATION[2 >>> 0] -> :two
              end

            :error ->
              :error
          end
        end
      end

    modules = mutate_marked_ast(__MODULE__.E1C3M0, marked)

    %{original: m_original, expected: m_expected, mutated: m_mutated} = modules

    right_mutation = {m_mutated, 3, 0}

    # -------------------------
    # No mutations
    # -------------------------
    # The original module should be equivalent to the mutated modules
    # (the mutations in the mutated module are activated conditionally)
    assert m_original.f({:ok, 1}) == :one
    assert m_mutated.f({:ok, 1}) == :one

    assert m_original.f({:ok, 2}) == :two
    assert m_mutated.f({:ok, 2}) == :two

    assert m_original.f(:error) == :error
    assert m_mutated.f(:error) == :error

    assert_raise CaseClauseError, fn -> m_original.f(:invalid) end
    assert_raise CaseClauseError, fn -> m_mutated.f(:invalid) end

    assert_raise CaseClauseError, fn -> m_original.f({:ok, :invalid}) end
    assert_raise CaseClauseError, fn -> m_mutated.f({:ok, :invalid}) end

    # The expected module should have the mutation "always active"
    assert m_expected.f({:ok, 1}) == :one
    assert_raise CaseClauseError, fn -> m_expected.f({:ok, 2}) == :two end
    assert m_expected.f({:ok, 0}) == :two
    assert m_expected.f(:error) == :error
    assert_raise CaseClauseError, fn -> m_expected.f(:invalid) end
    assert_raise CaseClauseError, fn -> m_expected.f({:ok, :invalid}) end

    # ----------------------------------------------
    # Mutated module with the wrong mutation
    # ----------------------------------------------
    # Should be equivalent to the original module)

    ActiveMutation.with_mutation(@invalid_mutation, fn ->
      m_original.f({:ok, 1}) <~> m_mutated.f({:ok, 1})
      m_original.f({:ok, 1}) <~> m_mutated.f({:ok, 1})
      m_original.f({:ok, 2}) <~> m_mutated.f({:ok, 2})
      m_original.f(:error) <~> m_mutated.f(:error)
      m_original.f(:invalid) <~> m_mutated.f(:invalid)
      m_original.f({:ok, :invalid}) <~> m_mutated.f({:ok, :invalid})
    end)

    # ------------------------------------------------
    # Mutated module with the right mutation
    # ------------------------------------------------
    # Should be equivalent to the expected module

    ActiveMutation.with_mutation(right_mutation, fn ->
      m_mutated.f({:ok, 1}) <~> m_expected.f({:ok, 1})
      m_mutated.f({:ok, 1}) <~> m_expected.f({:ok, 1})
      m_mutated.f({:ok, 2}) <~> m_expected.f({:ok, 2})
      m_mutated.f(:error) <~> m_expected.f(:error)
      m_mutated.f(:invalid) <~> m_expected.f(:invalid)
      m_mutated.f({:ok, :invalid}) <~> m_expected.f({:ok, :invalid})
    end)
  end

  test "example #1 - Codon 4, mutation 0" do
    marked =
      quote do
        def f(x) do
          case x do
            {:ok, value} ->
              case value do
                1 -> :one
                2 -> MUTATION[:two >>> :darwin_was_here]
              end

            :error ->
              :error
          end
        end
      end

    modules = mutate_marked_ast(__MODULE__.E1C4M0, marked)

    %{original: m_original, expected: m_expected, mutated: m_mutated} = modules

    right_mutation = {m_mutated, 4, 0}

    # -------------------------
    # No mutations
    # -------------------------
    # The original module should be equivalent to the mutated modules
    # (the mutations in the mutated module are activated conditionally)
    assert m_original.f({:ok, 1}) == :one
    assert m_mutated.f({:ok, 1}) == :one

    assert m_original.f({:ok, 2}) == :two
    assert m_mutated.f({:ok, 2}) == :two

    assert m_original.f(:error) == :error
    assert m_mutated.f(:error) == :error

    assert_raise CaseClauseError, fn -> m_original.f(:invalid) end
    assert_raise CaseClauseError, fn -> m_mutated.f(:invalid) end

    assert_raise CaseClauseError, fn -> m_original.f({:ok, :invalid}) end
    assert_raise CaseClauseError, fn -> m_mutated.f({:ok, :invalid}) end

    # The expected module should have the mutation "always active"
    assert m_expected.f({:ok, 1}) == :one
    assert m_expected.f({:ok, 2}) == :darwin_was_here
    assert m_expected.f(:error) == :error
    assert_raise CaseClauseError, fn -> m_expected.f(:invalid) end
    assert_raise CaseClauseError, fn -> m_expected.f({:ok, :invalid}) end

    # ----------------------------------------------
    # Mutated module with the wrong mutation
    # ----------------------------------------------
    # Should be equivalent to the original module)

    ActiveMutation.with_mutation(@invalid_mutation, fn ->
      m_original.f({:ok, 1}) <~> m_mutated.f({:ok, 1})
      m_original.f({:ok, 1}) <~> m_mutated.f({:ok, 1})
      m_original.f({:ok, 2}) <~> m_mutated.f({:ok, 2})
      m_original.f(:error) <~> m_mutated.f(:error)
      m_original.f(:invalid) <~> m_mutated.f(:invalid)
      m_original.f({:ok, :invalid}) <~> m_mutated.f({:ok, :invalid})
    end)

    # ------------------------------------------------
    # Mutated module with the right mutation
    # ------------------------------------------------
    # Should be equivalent to the expected module

    ActiveMutation.with_mutation(right_mutation, fn ->
      m_mutated.f({:ok, 1}) <~> m_expected.f({:ok, 1})
      m_mutated.f({:ok, 1}) <~> m_expected.f({:ok, 1})
      m_mutated.f({:ok, 2}) <~> m_expected.f({:ok, 2})
      m_mutated.f(:error) <~> m_expected.f(:error)
      m_mutated.f(:invalid) <~> m_expected.f(:invalid)
      m_mutated.f({:ok, :invalid}) <~> m_expected.f({:ok, :invalid})
    end)
  end

  test "example #1 - Codon 5, mutation 0" do
    marked =
      quote do
        def f(x) do
          case x do
            {:ok, value} ->
              case value do
                1 -> :one
                2 -> :two
              end

            MUTATION[:error >>> :darwin_was_here] ->
              :error
          end
        end
      end

    modules = mutate_marked_ast(__MODULE__.E1C5M0, marked)

    %{original: m_original, expected: m_expected, mutated: m_mutated} = modules

    right_mutation = {m_mutated, 5, 0}

    # -------------------------
    # No mutations
    # -------------------------
    # The original module should be equivalent to the mutated modules
    # (the mutations in the mutated module are activated conditionally)
    assert m_original.f({:ok, 1}) == :one
    assert m_mutated.f({:ok, 1}) == :one

    assert m_original.f({:ok, 2}) == :two
    assert m_mutated.f({:ok, 2}) == :two

    assert m_original.f(:error) == :error
    assert m_mutated.f(:error) == :error

    assert_raise CaseClauseError, fn -> m_original.f(:invalid) end
    assert_raise CaseClauseError, fn -> m_mutated.f(:invalid) end

    assert_raise CaseClauseError, fn -> m_original.f({:ok, :invalid}) end
    assert_raise CaseClauseError, fn -> m_mutated.f({:ok, :invalid}) end

    # The expected module should have the mutation "always active"
    assert m_expected.f({:ok, 1}) == :one
    assert m_expected.f({:ok, 2}) == :two
    assert_raise CaseClauseError, fn -> m_expected.f(:error) end
    assert m_expected.f(:darwin_was_here) == :error
    assert_raise CaseClauseError, fn -> m_expected.f(:invalid) end
    assert_raise CaseClauseError, fn -> m_expected.f({:ok, :invalid}) end

    # ----------------------------------------------
    # Mutated module with the wrong mutation
    # ----------------------------------------------
    # Should be equivalent to the original module)

    ActiveMutation.with_mutation(@invalid_mutation, fn ->
      m_original.f({:ok, 1}) <~> m_mutated.f({:ok, 1})
      m_original.f({:ok, 1}) <~> m_mutated.f({:ok, 1})
      m_original.f({:ok, 2}) <~> m_mutated.f({:ok, 2})
      m_original.f(:error) <~> m_mutated.f(:error)
      m_original.f(:invalid) <~> m_mutated.f(:invalid)
      m_original.f({:ok, :invalid}) <~> m_mutated.f({:ok, :invalid})
    end)

    # ------------------------------------------------
    # Mutated module with the right mutation
    # ------------------------------------------------
    # Should be equivalent to the expected module

    ActiveMutation.with_mutation(right_mutation, fn ->
      m_mutated.f({:ok, 1}) <~> m_expected.f({:ok, 1})
      m_mutated.f({:ok, 1}) <~> m_expected.f({:ok, 1})
      m_mutated.f({:ok, 2}) <~> m_expected.f({:ok, 2})
      m_mutated.f(:error) <~> m_expected.f(:error)
      m_mutated.f(:invalid) <~> m_expected.f(:invalid)
      m_mutated.f({:ok, :invalid}) <~> m_expected.f({:ok, :invalid})
    end)
  end

  test "example #1 - Codon 6, mutation 0" do
    marked =
      quote do
        def f(x) do
          case x do
            {:ok, value} ->
              case value do
                1 -> :one
                2 -> :two
              end

            :error ->
              MUTATION[:error >>> :darwin_was_here]
          end
        end
      end

    modules = mutate_marked_ast(__MODULE__.E1C6M0, marked)

    %{original: m_original, expected: m_expected, mutated: m_mutated} = modules

    right_mutation = {m_mutated, 6, 0}

    # -------------------------
    # No mutations
    # -------------------------
    # The original module should be equivalent to the mutated modules
    # (the mutations in the mutated module are activated conditionally)
    assert m_original.f({:ok, 1}) == :one
    assert m_mutated.f({:ok, 1}) == :one

    assert m_original.f({:ok, 2}) == :two
    assert m_mutated.f({:ok, 2}) == :two

    assert m_original.f(:error) == :error
    assert m_mutated.f(:error) == :error

    assert_raise CaseClauseError, fn -> m_original.f(:invalid) end
    assert_raise CaseClauseError, fn -> m_mutated.f(:invalid) end

    assert_raise CaseClauseError, fn -> m_original.f({:ok, :invalid}) end
    assert_raise CaseClauseError, fn -> m_mutated.f({:ok, :invalid}) end

    # The expected module should have the mutation "always active"
    assert m_expected.f({:ok, 1}) == :one
    assert m_expected.f({:ok, 2}) == :two
    assert m_expected.f(:error) == :darwin_was_here
    assert_raise CaseClauseError, fn -> m_expected.f(:invalid) end
    assert_raise CaseClauseError, fn -> m_expected.f({:ok, :invalid}) end

    # ----------------------------------------------
    # Mutated module with the wrong mutation
    # ----------------------------------------------
    # Should be equivalent to the original module)

    ActiveMutation.with_mutation(@invalid_mutation, fn ->
      m_original.f({:ok, 1}) <~> m_mutated.f({:ok, 1})
      m_original.f({:ok, 1}) <~> m_mutated.f({:ok, 1})
      m_original.f({:ok, 2}) <~> m_mutated.f({:ok, 2})
      m_original.f(:error) <~> m_mutated.f(:error)
      m_original.f(:invalid) <~> m_mutated.f(:invalid)
      m_original.f({:ok, :invalid}) <~> m_mutated.f({:ok, :invalid})
    end)

    # ------------------------------------------------
    # Mutated module with the right mutation
    # ------------------------------------------------
    # Should be equivalent to the expected module

    ActiveMutation.with_mutation(right_mutation, fn ->
      m_mutated.f({:ok, 1}) <~> m_expected.f({:ok, 1})
      m_mutated.f({:ok, 1}) <~> m_expected.f({:ok, 1})
      m_mutated.f({:ok, 2}) <~> m_expected.f({:ok, 2})
      m_mutated.f(:error) <~> m_expected.f(:error)
      m_mutated.f(:invalid) <~> m_expected.f(:invalid)
      m_mutated.f({:ok, :invalid}) <~> m_expected.f({:ok, :invalid})
    end)
  end

  test "example #1 - XXXX" do
    marked =
      quote do
        def f(x) do
          case x do
            {:ok, value} ->
              case value do
                1 -> :one
                2 -> :two
              end

            :error ->
              MUTATION[:error >>> :darwin_was_here]
          end
        end
      end

    modules = mutate_marked_ast(__MODULE__.E1C6M0, marked)

    %{original: m_original, expected: m_expected, mutated: m_mutated} = modules

    right_mutation = {m_mutated, 6, 0}

    # -------------------------
    # No mutations
    # -------------------------
    # The original module should be equivalent to the mutated modules
    # (the mutations in the mutated module are activated conditionally)
    assert m_original.f({:ok, 1}) == :one
    assert m_mutated.f({:ok, 1}) == :one

    assert m_original.f({:ok, 2}) == :two
    assert m_mutated.f({:ok, 2}) == :two

    assert m_original.f(:error) == :error
    assert m_mutated.f(:error) == :error

    assert_raise CaseClauseError, fn -> m_original.f(:invalid) end
    assert_raise CaseClauseError, fn -> m_mutated.f(:invalid) end

    assert_raise CaseClauseError, fn -> m_original.f({:ok, :invalid}) end
    assert_raise CaseClauseError, fn -> m_mutated.f({:ok, :invalid}) end

    # The expected module should have the mutation "always active"
    assert m_expected.f({:ok, 1}) == :one
    assert m_expected.f({:ok, 2}) == :two
    assert m_expected.f(:error) == :darwin_was_here
    assert_raise CaseClauseError, fn -> m_expected.f(:invalid) end
    assert_raise CaseClauseError, fn -> m_expected.f({:ok, :invalid}) end

    # ----------------------------------------------
    # Mutated module with the wrong mutation
    # ----------------------------------------------
    # Should be equivalent to the original module)

    ActiveMutation.with_mutation(@invalid_mutation, fn ->
      m_original.f({:ok, 1}) <~> m_mutated.f({:ok, 1})
      m_original.f({:ok, 1}) <~> m_mutated.f({:ok, 1})
      m_original.f({:ok, 2}) <~> m_mutated.f({:ok, 2})
      m_original.f(:error) <~> m_mutated.f(:error)
      m_original.f(:invalid) <~> m_mutated.f(:invalid)
      m_original.f({:ok, :invalid}) <~> m_mutated.f({:ok, :invalid})
    end)

    # ------------------------------------------------
    # Mutated module with the right mutation
    # ------------------------------------------------
    # Should be equivalent to the expected module

    ActiveMutation.with_mutation(right_mutation, fn ->
      m_mutated.f({:ok, 1}) <~> m_expected.f({:ok, 1})
      m_mutated.f({:ok, 1}) <~> m_expected.f({:ok, 1})
      m_mutated.f({:ok, 2}) <~> m_expected.f({:ok, 2})
      m_mutated.f(:error) <~> m_expected.f(:error)
      m_mutated.f(:invalid) <~> m_expected.f(:invalid)
      m_mutated.f({:ok, :invalid}) <~> m_expected.f({:ok, :invalid})
    end)
  end
end
