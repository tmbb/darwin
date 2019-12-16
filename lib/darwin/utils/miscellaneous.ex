defmodule Darwin.Utils.Miscellaneous do
  alias Darwin.ExToErl

  @moduledoc false

  def section_between_markers(path, marker) do
    contents = File.read!(path)

    match =
      Regex.named_captures(
        ~r/\n +#! START #{marker}(?<code>(\n(?! +#! END #{marker})[^\n]*)*)\n +#! END #{marker}/,
        contents
      )

    %{"code" => code} = match

    code
  end

  def section_to_erlang_abstract_code(path, marker) do
    code = section_between_markers(path, marker)
    ExToErl.elixir_module_source_to_erlang_abstract_code(ModuleA, code)
  end

  def section_to_erlang_source(path, marker) do
    code = section_between_markers(path, marker)
    ExToErl.elixir_module_source_to_erlang_source(ModuleA, code)
  end
end
