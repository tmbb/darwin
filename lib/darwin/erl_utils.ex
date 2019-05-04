defmodule Darwin.ErlUtils do
  def expression!(bin) do
    charlist = String.to_charlist(bin)
    {:ok, tokens, _} = :erl_scan.string(charlist)
    {:ok, [expression]} = :erl_parse.parse_exprs(tokens)
    expression
  end

  def pprint(abstract_code) do
    [:erl_prettypr.format(abstract_code), "\n"]
    |> to_string()
    |> String.replace("\t", "        ")
  end
end
