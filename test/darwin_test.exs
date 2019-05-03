defmodule DarwinTest do
  use ExUnit.Case
  doctest Darwin

  # Strict boolean operators

  test "operator: ||" do
    assert quote(do: x || y) |> Darwin.mutate() |> Darwin.format() == """
           (fn a, b ->
              case(Darwin.ActiveMutation.get()) do
                0 ->
                  a && b

                _other ->
                  a || b
              end
            end).(x, y)
           """
  end

  test "operator: &&" do
    assert quote(do: x && y) |> Darwin.mutate() |> Darwin.format() == """
           (fn a, b ->
              case(Darwin.ActiveMutation.get()) do
                0 ->
                  a || b

                _other ->
                  a && b
              end
            end).(x, y)
           """
  end

  # Permissive boolean operators

  test "operator: and" do
    assert quote(do: x and y) |> Darwin.mutate() |> Darwin.format() == """
           (fn a, b ->
              case(Darwin.ActiveMutation.get()) do
                0 ->
                  a or b

                _other ->
                  a and b
              end
            end).(x, y)
           """
  end

  test "operator: or" do
    assert quote(do: x or y) |> Darwin.mutate() |> Darwin.format() == """
           (fn a, b ->
              case(Darwin.ActiveMutation.get()) do
                0 ->
                  a and b

                _other ->
                  a or b
              end
            end).(x, y)
           """
  end

  test "if statement" do
    if_statement =
      quote do
        if p do
          x
        else
          y
        end
      end

    assert if_statement
           |> Darwin.mutate()
           |> Darwin.format() == """
           if(
             case(Darwin.ActiveMutation.get()) do
               0 ->
                 true

               1 ->
                 false

               2 ->
                 !p

               _other ->
                 p
             end
           ) do
             x
           else
             y
           end
           """
  end
end
