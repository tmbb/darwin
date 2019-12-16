defmodule Darwin.Erlang do
  @moduledoc """
  Utilities to work with Erlang expressions.
  """

  require Logger
  import ExUnit.Assertions

  @doc """
  Parses a literal binary as Erlang into abstract code and replaces variable
  occurrences according to the substitutions given as a keyword list.
  """
  defmacro interpolate_in_abstract_code!(bin, substitutions) do
    # Parse the expression out of the binary
    erlang_abstract_code = forms!(bin)
    escaped_erlang_abstract_code = Macro.escape(erlang_abstract_code)
    # Replace the variables by the substitutions
    quote do
      :parse_trans.plain_transform(
        Darwin.Erlang.variable_replacer(unquote(substitutions)),
        unquote(escaped_erlang_abstract_code)
      )
    end
  end

  @doc false
  def variable_replacer(substitutions) do
    fn
      {:var, _line, name} ->
        case Keyword.fetch(substitutions, name) do
          {:ok, value} ->
            value

          :error ->
            :continue
        end

      _ ->
        :continue
    end
  end

  @doc """
  Parses an expression into erlang abstract code

  Raises if the binary is invalid erlang.
  """
  def expression!(bin) do
    charlist = String.to_charlist(bin)
    {:ok, tokens, _} = :erl_scan.string(charlist)
    {:ok, [expression]} = :erl_parse.parse_exprs(tokens)
    expression
  end

  @doc """
  Parses a string into a form list.

  Raises if the binary is invalid erlang.
  """
  def forms!(bin) do
    charlist = String.to_charlist(bin)
    {:ok, tokens, _} = :erl_scan.string(charlist)
    {:ok, forms} = :erl_parse.parse_exprs(tokens)
    forms
  end

  @doc """
  Asserts that two expressions (`left` and `right`) are equivalent.

  An expression can be a term representing Erlang abstract code or a binary
  containing valid Erlang code.
  """
  def assert_equivalent(left, right) do
    left_forms = if is_binary(left), do: forms!(left), else: List.wrap(left)
    right_forms = if is_binary(right), do: forms!(right), else: List.wrap(right)

    canonical_left = pprint_forms(left_forms)
    canonical_right = pprint_forms(right_forms)

    assert(canonical_left == canonical_right)
  end

  @doc """
  Tests whether two expressions (`left` and `right`) are equivalent.

  An expression can be a term representing Erlang abstract code or a binary
  containing valid Erlang code.
  """
  def equivalent?(left, right) do
    left_forms = if is_binary(left), do: forms!(left), else: List.wrap(left)
    right_forms = if is_binary(right), do: forms!(right), else: List.wrap(right)

    canonical_left = pprint_forms(left_forms)
    canonical_right = pprint_forms(right_forms)

    canonical_left == canonical_right
  end

  @doc """
  Pretty prints erlang abstract code into the erlang source
  """
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

  @doc """
  Gets the erlang source code from the BEAM file
  """
  def beam_to_erlang_source(module) do
    filename = :code.which(module)

    {:ok, {_, [{:abstract_code, {_, abstract_code}}]}} =
      :beam_lib.chunks(filename, [:abstract_code])

    pprint(:erl_syntax.form_list(abstract_code))
  end

  @doc """
  Gets the erlang source code from the BEAM file and writes it into a file.
  """
  def beam_to_erlang_source_file(module, file) do
    source = beam_to_erlang_source(module)
    File.write!(file, source)
  end
end
