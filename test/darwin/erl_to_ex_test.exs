defmodule Darwin.ErlToExTest do
  use ExUnit.Case, async: true

  alias Darwin.ExToErl
  alias Darwin.ErlToEx

  test "special case - elem/2: #1" do
    elixir_original = quote(do: elem(tup, n))
    erlang = ExToErl.elixir_ast_to_erlang_abstract_code(elixir_original)
    elixir_final = ErlToEx.erl_to_ex(erlang)

    elixir_assert_equivalent(elixir_original, elixir_final)
  end

  test "special case - elem/2: #2" do
    elixir_original = quote(do: elem(tup, 3))
    erlang = ExToErl.elixir_ast_to_erlang_abstract_code(elixir_original)
    elixir_final = ErlToEx.erl_to_ex(erlang)

    elixir_assert_equivalent(elixir_original, elixir_final)
  end

  test "kernel functions: is_boolean/1" do
    assert_roundtrip_is_correct(quote(do: is_boolean(arg)))
  end

  test "kernel functions: is_float/1" do
    assert_roundtrip_is_correct(quote(do: is_float(arg)))
  end

  test "kernel functions: is_integer/1" do
    assert_roundtrip_is_correct(quote(do: is_integer(arg)))
  end

  test "kernel functions: is_list/1" do
    assert_roundtrip_is_correct(quote(do: is_list(arg)))
  end

  test "kernel functions: is_number/1" do
    assert_roundtrip_is_correct(quote(do: is_number(arg)))
  end

  test "kernel functions: is_pid/1" do
    assert_roundtrip_is_correct(quote(do: is_pid(arg)))
  end

  test "kernel functions: is_port/1" do
    assert_roundtrip_is_correct(quote(do: is_port(arg)))
  end

  test "kernel functions: is_reference/1" do
    assert_roundtrip_is_correct(quote(do: is_reference(arg)))
  end

  test "kernel functions: is_tuple/1" do
    assert_roundtrip_is_correct(quote(do: is_tuple(arg)))
  end

  test "kernel functions: is_function/2" do
    assert_roundtrip_is_correct(quote(do: is_function(f, a)))
  end

  # Utilities
  def assert_roundtrip_is_correct(elixir_original) do
    erlang = ExToErl.elixir_ast_to_erlang_abstract_code(elixir_original)
    elixir_final = ErlToEx.erl_to_ex(erlang)

    elixir_assert_equivalent(elixir_original, elixir_final)
  end

  def elixir_assert_equivalent(left, right) do
    left_quoted =
      if is_binary(left),
        do: Code.string_to_quoted!(left),
        else: left

    right_quoted =
      if is_binary(right),
        do: Code.string_to_quoted!(right),
        else: right

    canonical_left =
      left_quoted
      |> Macro.to_string()
      |> Code.format_string!()
      |> to_string()

    canonical_right =
      right_quoted
      |> Macro.to_string()
      |> Code.format_string!()
      |> to_string()

    assert(canonical_left == canonical_right)
  end
end
