defmodule Darwin.MixProject do
  use Mix.Project

  def project do
    [
      app: :darwin,
      version: "0.1.0",
      elixir: "~> 1.7",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      elixirc_paths: elixirc_paths(Mix.env())
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/common"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:benchee, "~> 1.0.1"},
      {:decimal, "~> 1.0"},
      {:beam_to_ex_ast, "~> 0.3.3"},
      {:stream_data, "~> 0.4.3"},
      {:ex_to_erl, path: "../../ex_to_erl"},
      {:ex_doc, github: "elixir-lang/ex_doc", only: [:dev, :doc]},
      {:assert_value, "~> 0.9.2"}
    ]
  end
end
