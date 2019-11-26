defmodule Darwin.Utils.TimeConvert do
  @moduledoc false

  @minute 60
  @hour @minute * 60
  @day @hour * 24
  @week @day * 7
  @divisor [@week, @day, @hour, @minute, 1]

  defp sec_to_str(sec) do
    {_, [s, m, h, d, w]} =
      Enum.reduce(@divisor, {sec, []}, fn divisor, {n, acc} ->
        {rem(n, divisor), [n / divisor | acc]}
      end)

    [
      "#{trunc(w)}wk",
      "#{trunc(d)}d",
      "#{trunc(h)}h",
      "#{trunc(m)}m",
      "#{Float.round(s, 3)}s"
    ]
    |> Enum.reject(fn str ->
      str == "0.0" or (String.starts_with?(str, "0") and not String.starts_with?(str, "0."))
    end)
    |> Enum.join(", ")
  end

  def microsec_to_str(ms), do: trunc(ms / 1_000_000) |> sec_to_str()
end
