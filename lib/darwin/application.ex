defmodule Darwin.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      # Meke it so that Darwin.ExToErl works
      Darwin.ExToErl.ModuleQueue
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: Darwin.Supervisor)
  end
end
