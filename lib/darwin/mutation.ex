defmodule Darwin.Mutation do
  defstruct [
    :type,
    :message
  ]

  def new(opts) do
    struct(__MODULE__, opts)
  end
end
