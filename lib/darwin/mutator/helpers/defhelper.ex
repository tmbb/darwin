defmodule Darwin.Mutator.Helpers.DefHelper do
  defmacro defhelper(name, arguments, do: body) do
    function_name = :"do_#{name}"

    quote do
      def unquote(name)(unquote_splicing(arguments)) do
        Darwin.Mutator.Helpers.call_remote(
          Darwin.Mutator.Helpers,
          unquote(function_name),
          unquote(arguments)
        )
      end

      @doc false
      def unquote(function_name)(unquote(arguments)) do
        unquote(body)
      end
    end
  end
end
