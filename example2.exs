def(f(x)) do
  case(x) do
    {:ok, value} ->
      case(value) do
        1 ->
          :one

        2 ->
          :two
      end

    :error ->
      :error
  end
end