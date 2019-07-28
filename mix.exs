defmodule Darwin.MixProject do
  use Mix.Project

  def project do
    [
      app: :darwin,
      version: "0.1.0",
      elixir: "~> 1.7",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:benchee, "~> 1.0.1"},
      {:decimal, "~> 1.0"},
      {:beam_to_ex_ast, "~> 0.3.3"},
      {:ex_doc, github: "elixir-lang/ex_doc", only: [:dev, :doc]}
    ]
  end
end
