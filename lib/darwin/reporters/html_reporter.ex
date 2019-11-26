defmodule Darwin.Reporters.HtmlReporter do
  alias Darwin.TestRunner.Persistence
  alias Darwin.TestRunner.Persistence.Mutant
  alias Darwin.Utils.SafeSourcePath
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

  def run() do
    mutants = Persistence.all_mutants() |> IO.inspect(label: "mutants")
    grouped = Mutant.group_by_module_and_line(mutants)

    dst = @output_path

    make_output_dir(dst)

    for {module, _lines} <- grouped do
      code_path = SafeSourcePath.source_path_for_module(module) |> IO.inspect(label: "code_path")

      if code_path do
        if String.ends_with?(code_path, ".ex") do
          code = File.read!(code_path)
          highlighted = Makeup.highlight_inner_html(code)
          contents = layout_html(module: module, highlighted: highlighted)
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
