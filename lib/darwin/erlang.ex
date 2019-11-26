defmodule Darwin.Erlang do
  @moduledoc """
  Utilities to work with Erlang expressions.
  """

  @doc "Parses an expression into erlang abstract code"
  def expression!(bin) do
    charlist = String.to_charlist(bin)
    {:ok, tokens, _} = :erl_scan.string(charlist)
    {:ok, [expression]} = :erl_parse.parse_exprs(tokens)
    expression
  end

  @doc """
  Parses a string into a form list.
  """
  def forms!(bin) do
    charlist = String.to_charlist(bin)
    {:ok, tokens, _} = :erl_scan.string(charlist)
    {:ok, forms} = :erl_parse.parse_exprs(tokens)
    forms
  end

  @doc """
  Tests whether two expressions (`left` and `right`) are equivalent.

  An expression can be a term representing Erlang abstract code or a binary
  containing valid Erlang code.
  """
  def equivalent?(left, right) do
    left_expression = if is_binary(left), do: expression!(left), else: left
    right_expression = if is_binary(right), do: expression!(right), else: right

    canonical_left = pprint(left_expression)
    canonical_right = pprint(right_expression)
    canonical_left == canonical_right
  end

  @doc "Pretty prints erlang abstract code into the erlang source"
  def pprint(abstract_code, opts \\ []) do
    indent = Keyword.get(opts, :indent, 8)

    [:erl_prettypr.format(abstract_code), "\n"]
    |> IO.iodata_to_binary()
    |> String.replace("\t", String.duplicate(" ", indent))
  end

  defp append_newline(value), do: [value | "\n"]

  def pprint_forms(form_list, opts \\ []) do
    indent = Keyword.get(opts, :indent, 8)

    :erl_syntax.form_list(form_list)
    |> :erl_prettypr.format()
    |> append_newline()
    |> IO.iodata_to_binary()
    |> String.replace("\t", String.duplicate(" ", indent))
  end

  @doc "gets the erlang source code from the BEAM file"
  def beam_to_erlang_source(module) do
    filename = :code.which(module)

    {:ok, {_, [{:abstract_code, {_, abstract_code}}]}} =
      :beam_lib.chunks(filename, [:abstract_code])

    pprint(:erl_syntax.form_list(abstract_code))
  end

  @doc "gets the erlang source code from the BEAM file"
  def beam_to_erlang_source_file(module, file) do
    source = beam_to_erlang_source(module)
    File.write!(file, source)
  end
end
