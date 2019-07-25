defmodule Darwin.ErlUtils.AbstractCode do
  def call_mfa({module, fun, args}, line) do
    {:call, line, {:remote, line, {:atom, line, module}, {:atom, line, fun}}, args}
  end

  @doc "Encodes an atom into Erlang abstract code"
  def encode_atom(atom, line \\ 0) when is_atom(atom), do: {:atom, line, atom}

  @doc "Encodes a float into Erlang abstract code"
  def encode_float(float, line \\ 0) when is_float(float), do: {:float, line, float}

  @doc "Encodes an integer into Erlang abstract code"
  def encode_integer(integer, line \\ 0) when is_integer(integer), do: {:integer, line, integer}
end
