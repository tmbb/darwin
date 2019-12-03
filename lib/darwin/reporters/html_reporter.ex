defmodule Darwin.Reporters.HtmlReporter do
  alias Darwin.Mutator.Mutant
  alias Makeup.Lexers.ElixirLexer
  alias Makeup.Formatters.HTML.HTMLFormatter
  alias Makeup.Lexer
  require EEx

  @external_resource "lib/darwin/reporters/html_reporter/templates/layout.html.eex"
  @output_path "darwin/reports/html"

  EEx.function_from_file(
    :defp,
    :layout_html,
    "lib/darwin/reporters/html_reporter/templates/layout.html.eex",
    [:assigns]
  )

  def make_output_dir(root) do
    File.mkdir_p!(@output_path)
    File.cp_r!(src_root_path(), root)
  end

  def format_elixir(ast) do
    code =
      ast
      |> Macro.to_string()
      |> Code.format_string!()
      |> IO.iodata_to_binary()

    # highlighted = Makeup.highlight_inner_html(code)

    # "<code class=\"highlight\">#{highlighted}</code>"

    code
  end

  defp format_mutant_state(%Mutant{} = mutant) do
    case mutant.state do
      :killed -> "Killed"
      :survived -> "<strong>Survived</strong>"
    end
  end

  def nr_of_digits(n) do
    n_string = Integer.to_string(n)
    byte_size(n_string)
  end

  def left_pad(n, length) do
    padding = String.duplicate(" ", length - nr_of_digits(n))
    [padding, Integer.to_string(n)]
  end

  defp highlight_lines(code) do
    tokens = ElixirLexer.lex(code)
    token_lines = Lexer.split_into_lines(tokens)

    max_line_nr = length(token_lines)
    padding_length = nr_of_digits(max_line_nr)

    lines =
      for {token_line, line_nr} <- Enum.with_index(token_lines, 1) do
        [
          ~s[<a class="line" name="L],
          to_string(line_nr),
          ~s["><span>],
          left_pad(line_nr, padding_length),
          "  </span><span>",
          HTMLFormatter.format_inner_as_iolist(token_line, []),
          ~s[</span></a>\n]
        ]
      end

    IO.iodata_to_binary(lines)
  end

  def run(code_paths, grouped_mutants) do
    dst = @output_path
    make_output_dir(@output_path)

    for {module, mutants} <- grouped_mutants do
      code_path = Map.get(code_paths, module)

      if code_path do
        if String.ends_with?(code_path, ".ex") do
          code = File.read!(code_path)
          highlighted = highlight_lines(code)

          contents =
            layout_html(
              module: module,
              highlighted: highlighted,
              mutants: mutants
            )

          base_path = inspect(module) <> ".html"
          report_path = Path.join(dst, base_path)
          File.write!(report_path, contents)
        end
      end
    end

    :ok
  end

  defp src_root_path() do
    Path.join(:code.priv_dir(:darwin), "reporters/html_reporter")
  end
end
