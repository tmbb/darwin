defmodule Darwin.ErlToEx do
  @moduledoc false
  import Darwin.Erlang, only: [e!: 2]

  @ellipse quote(do: ...)

  def erl_variable_name_to_ex_variable_name(erl_var_name) do
    case String.match?(erl_var_name, ~r/@|_/) do
      true ->
        case Regex.run(~r/_([^@_]+)@.*/, erl_var_name) do
          [_, match] -> match
          _ -> "..."
        end

      false ->
        Macro.underscore(erl_var_name)
    end
  end

  @doc """
  Convert erlang abstract code into equivalent Elixir AST
  """
  def erl_to_ex({:var, _loc, erl_var}) when is_atom(erl_var) do
    erl_var_name = Atom.to_string(erl_var)
    ex_var_name = erl_variable_name_to_ex_variable_name(erl_var_name)
    ex_var = String.to_atom(ex_var_name)
    {ex_var, [], nil}
  end

  def erl_to_ex({:op, _loc, erl_op, left, right}) do
    ex_op = op_erl_to_ex(erl_op)
    ex_left = erl_to_ex(left)
    ex_right = erl_to_ex(right)
    {ex_op, [], [ex_left, ex_right]}
  end

  def erl_to_ex({:atom, _meta, true}), do: quote(do: true)
  def erl_to_ex({:atom, _meta, false}), do: quote(do: false)
  def erl_to_ex({:atom, _meta, nil}), do: quote(do: nil)

  def erl_to_ex({:atom, _meta, atom}), do: atom
  def erl_to_ex({:string, _meta, charlist}), do: charlist
  def erl_to_ex({:integer, _meta, integer}), do: integer
  def erl_to_ex({:float, _meta, float}), do: float

  def erl_to_ex({nil, _line}), do: quote(do: [])

  def erl_to_ex({:bin, _meta, patterns}) do
    patterns_ex = Enum.map(patterns, &erl_to_ex/1)
    quote(do: <<unquote_splicing(patterns_ex)>>)
  end

  def erl_to_ex({:bin_element, _meta, value, size, :default}) do
    value_ex = erl_to_ex(value)
    size_ex = erl_to_ex(size)
    quote(do: unquote(value_ex) :: size(unquote(size_ex)))
  end

  def erl_to_ex({:bin_element, _meta, _value, _size, _type}), do: @ellipse

  @func_names_arity_1 [
    :is_boolean,
    :is_float,
    :is_function,
    :is_integer,
    :is_list,
    :is_number,
    :is_pid,
    :is_port,
    :is_record,
    :is_reference,
    :is_tuple
  ]

  @func_names_arity_2 [
    :is_function
  ]

  def erl_to_ex(e!("erlang:element(N, T).", N: {:integer, _, n}, T: arg)) do
    ex_n = n - 1
    ex_arg = erl_to_ex(arg)

    quote do
      elem(unquote(ex_arg), unquote(ex_n))
    end
  end

  def erl_to_ex(e!("erlang:element(N + 1, T).", N: n, T: arg)) do
    ex_n = erl_to_ex(n)
    ex_arg = erl_to_ex(arg)

    quote do
      elem(unquote(ex_arg), unquote(ex_n))
    end
  end

  def erl_to_ex({
        :call,
        _,
        {:remote, _, {:atom, _, :erlang}, {:atom, _, name}},
        [arg]
      })
      when name in @func_names_arity_1 do
    ex_arg = erl_to_ex(arg)

    quote do
      unquote(name)(unquote(ex_arg))
    end
  end

  def erl_to_ex({
        :call,
        _,
        {:remote, _, {:atom, _, :erlang}, {:atom, _, name}},
        [arg1, arg2]
      })
      when name in @func_names_arity_2 do
    ex_arg1 = erl_to_ex(arg1)
    ex_arg2 = erl_to_ex(arg2)

    quote do
      unquote(name)(unquote(ex_arg1), unquote(ex_arg2))
    end
  end

  def erl_to_ex({:call, _line1, {:remote, _, m, f}, args}) when is_list(args) do
    args_ex = Enum.map(args, &erl_to_ex/1)
    m_ex = erl_to_ex(m)
    f_ex = erl_to_ex(f)
    quote(do: unquote(m_ex).unquote(f_ex)(unquote_splicing(args_ex)))
  end

  def erl_to_ex({:call, _line1, {:var, _, _} = f, args}) do
    ex_args = Enum.map(args, &erl_to_ex/1)
    ex_f = erl_to_ex(f)
    quote(do: unquote(ex_f).(unquote_splicing(ex_args)))
  end

  def erl_to_ex(_), do: @ellipse

  def op_erl_to_ex(op) when op == :"=<", do: :<=
  def op_erl_to_ex(op) when op == :"/=", do: :!=
  def op_erl_to_ex(op) when op == :"=/=", do: :!==
  def op_erl_to_ex(op) when op == :"=:=", do: :===
  def op_erl_to_ex(op), do: op
end
