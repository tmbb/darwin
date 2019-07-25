defmodule Darwin.ErlUtils do
  @doc "Parses an expression into erlang abstract code"
  def expression!(bin) do
    charlist = String.to_charlist(bin)
    {:ok, tokens, _} = :erl_scan.string(charlist)
    {:ok, [expression]} = :erl_parse.parse_exprs(tokens)
    expression
  end

  @doc "Pretty prints erlang abstract code into the erlang source"
  def pprint(abstract_code, opts \\ []) do
    indent = Keyword.get(opts, :indent, 8)

    [:erl_prettypr.format(abstract_code), "\n"]
    |> to_string()
    |> String.replace("\t", String.duplicate(" ", indent))
  end

  @doc "gets the erlang source code from the BEAM file"
  def beam_to_erl(module) do
    filename = :code.which(module)

    {:ok, {_, [{:abstract_code, {_, abstract_code}}]}} =
      :beam_lib.chunks(filename, [:abstract_code])

    pprint(:erl_syntax.form_list(abstract_code))
  end

  @doc "gets the erlang source code from the BEAM file"
  def beam_to_abstract_code(module) do
    filename = :code.which(module)

    {:ok, {_, [{:abstract_code, {_, abstract_code}}]}} =
      :beam_lib.chunks(filename, [:abstract_code])

    abstract_code
  end
end
