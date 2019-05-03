defmodule AstMutation do
  @quoted (quote do
             defmodule Mod2 do
               def id(x) do
                 x
               end

               # something
               def add(x, y) do
                 id(x) + id(y)
               end
             end
           end)

  @macroexpanded_and_quoted Macro.expand(
                              quote do
                                defmodule Mod2 do
                                  def id(x) do
                                    x
                                  end

                                  # something
                                  def add(x, y) do
                                    id(x) + id(y)
                                  end
                                end
                              end,
                              __ENV__
                            )

  def create_module() do
    quoted =
      quote do
        def id(x) do
          x
        end

        # something
        def add(x, y) do
          id(x) + id(y)
        end
      end

    Module.create(Mod2, quoted, Macro.Env.location(__ENV__))
  end

  def compile_quoted() do
    Code.eval_quoted(@quoted)
  end

  def compile_macroexpanded() do
    Code.eval_quoted(@macroexpanded_and_quoted)
  end
end

Benchee.run(%{
  "Module.create" => fn -> AstMutation.create_module() end,
  "quoted" => fn -> AstMutation.compile_quoted() end,
  "macroexpanded" => fn -> AstMutation.compile_macroexpanded() end
})
