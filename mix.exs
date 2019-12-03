defmodule Darwin.MixProject do
  use Mix.Project

  def project do
    [
      app: :darwin,
      version: "0.1.0",
      elixir: "~> 1.7",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      elixirc_paths: elixirc_paths(Mix.env()),
      guaxinim: [
        src: "lib",
        dst: "literate",
        project_title: "Darwin"
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    maybe_mod =
      if Mix.env() in [:test, :dev] do
        [
          registered: [Darwin.ExToErl.ModuleQueue],
          mod: {Darwin.Application, []}
        ]
      else
        []
      end

    [
      extra_applications: [:logger]
    ] ++ maybe_mod
  end

  defp elixirc_paths(:test), do: ["lib", "test/common"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:parse_trans, "~> 3.3"},
      {:makeup_elixir, "~> 0.14"},
      # Testing dependencies
      {:stream_data, "~> 0.4.3", only: [:dev, :test]},
      # Benchmarking dependencies
      {:benchee, "~> 1.0.1", only: [:dev]},
      # Documentation dependencies
      {:ex_doc, github: "elixir-lang/ex_doc", only: [:dev, :doc]}
    ]
  end
end
