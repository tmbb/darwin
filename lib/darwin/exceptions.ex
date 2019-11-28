defmodule Darwin.Exceptions do
  @moduledoc false
  def raise(message) do
    raise(Darwin.Exceptions.Error, message: message, mix: true)
  end
end
