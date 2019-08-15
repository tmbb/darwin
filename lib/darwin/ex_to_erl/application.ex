defmodule Darwin.ExToErl.Application do
  use Application

  def start(_type, _args) do
    children = [
      {Darwin.ExToErl.ModuleQueue, []}
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
