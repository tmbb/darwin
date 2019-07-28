defmodule Darwin.Erlang.AbstractCode do
  @type t() :: any()

  @doc "Encodes a remote function call into Erlang abstract code"
  def call_mfa({module, fun, args}, opts \\ []) do
    line = Keyword.get(opts, :line, 0)
    {:call, line, {:remote, line, {:atom, line, module}, {:atom, line, fun}}, args}
  end

  @doc "Encodes an atom into Erlang abstract code"
  def encode_atom(atom, opts \\ []) when is_atom(atom) do
    line = Keyword.get(opts, :line, 0)
    {:atom, line, atom}
  end

  @doc "Encodes a float into Erlang abstract code"
  def encode_float(float, opts \\ []) when is_float(float) do
    line = Keyword.get(opts, :line, 0)
    {:float, line, float}
  end

  @doc "Encodes an integer into Erlang abstract code"
  def encode_integer(integer, opts \\ []) when is_integer(integer) do
    line = Keyword.get(opts, :line, 0)
    {:integer, line, integer}
  end
end
