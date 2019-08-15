defmodule Darwin.DefaultMutators.AbstractCodeTests.FullModuleTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  import DarwinTest.Helpers
  # alias DarwinTest.Generators, as: Gen
  # alias Darwin.ActiveMutation
  alias Darwin.Erlang
  import AssertValue

  test "module - single function" do
    {form_list, _ctx} =
      mutate_elixir_module(__MODULE__.Mod1, """
        def f(x) do
          x + x
        end
      """)

    assert_value Erlang.pprint_forms(form_list, indent: 2) == """
                 -file(\"lib/darwin/ex_to_erl.ex\", 368).
    
                 -module('Elixir.Darwin.DefaultMutators.AbstractCodeTests.FullModuleTest.Mod1').
    
                 -compile([no_auto_import]).
    
                 -export(['__info__'/1, f/1]).
    
                 -spec '__info__'(attributes | compile | functions |
                      macros | md5 | module | deprecated) -> any().
    
                 '__info__'(module) ->
                     'Elixir.Darwin.DefaultMutators.AbstractCodeTests.FullModuleTest.Mod1';
                 '__info__'(functions) -> [{f, 1}];
                 '__info__'(macros) -> [];
                 '__info__'(Key = attributes) ->
                     erlang:get_module_info('Elixir.Darwin.DefaultMutators.AbstractCodeTests.FullModuleTest.Mod1',
                          Key);
                 '__info__'(Key = compile) ->
                     erlang:get_module_info('Elixir.Darwin.DefaultMutators.AbstractCodeTests.FullModuleTest.Mod1',
                          Key);
                 '__info__'(Key = md5) ->
                     erlang:get_module_info('Elixir.Darwin.DefaultMutators.AbstractCodeTests.FullModuleTest.Mod1',
                          Key);
                 '__info__'(deprecated) -> [].
    
                 f(_x@1) ->
                     'Elixir.Darwin.Mutators.Default.OpAddMutator':do_mutate('Elixir.Darwin.DefaultMutators.AbstractCodeTests.FullModuleTest.Mod1',
                                   0, _x@1, _x@1).
                 """
  end

  test "module - two functions" do
    {form_list, _ctx} =
      mutate_elixir_module(__MODULE__.Mod2, """
        def f(x, y, z) do
          x + y + z
        end

        def g(x, y) do
          (x + y) * (x - y)
        end
      """)

    assert_value Erlang.pprint_forms(form_list, indent: 2) == """
                 -file(\"lib/darwin/ex_to_erl.ex\", 368).
    
                 -module('Elixir.Darwin.DefaultMutators.AbstractCodeTests.FullModuleTest.Mod2').
    
                 -compile([no_auto_import]).
    
                 -export(['__info__'/1, f/3, g/2]).
    
                 -spec '__info__'(attributes | compile | functions |
                      macros | md5 | module | deprecated) -> any().
    
                 '__info__'(module) ->
                     'Elixir.Darwin.DefaultMutators.AbstractCodeTests.FullModuleTest.Mod2';
                 '__info__'(functions) -> [{f, 3}, {g, 2}];
                 '__info__'(macros) -> [];
                 '__info__'(Key = attributes) ->
                     erlang:get_module_info('Elixir.Darwin.DefaultMutators.AbstractCodeTests.FullModuleTest.Mod2',
                          Key);
                 '__info__'(Key = compile) ->
                     erlang:get_module_info('Elixir.Darwin.DefaultMutators.AbstractCodeTests.FullModuleTest.Mod2',
                          Key);
                 '__info__'(Key = md5) ->
                     erlang:get_module_info('Elixir.Darwin.DefaultMutators.AbstractCodeTests.FullModuleTest.Mod2',
                          Key);
                 '__info__'(deprecated) -> [].
    
                 f(_x@1, _y@1, _z@1) ->
                     'Elixir.Darwin.Mutators.Default.OpAddMutator':do_mutate('Elixir.Darwin.DefaultMutators.AbstractCodeTests.FullModuleTest.Mod2',
                                   1,
                                   'Elixir.Darwin.Mutators.Default.OpAddMutator':do_mutate('Elixir.Darwin.DefaultMutators.AbstractCodeTests.FullModuleTest.Mod2',
                                                 0,
                                                 _x@1,
                                                 _y@1),
                                   _z@1).
    
                 g(_x@1, _y@1) ->
                     'Elixir.Darwin.Mutators.Default.OpMulMutator':do_mutate('Elixir.Darwin.DefaultMutators.AbstractCodeTests.FullModuleTest.Mod2',
                                   4,
                                   'Elixir.Darwin.Mutators.Default.OpAddMutator':do_mutate('Elixir.Darwin.DefaultMutators.AbstractCodeTests.FullModuleTest.Mod2',
                                                 2,
                                                 _x@1,
                                                 _y@1),
                                   'Elixir.Darwin.Mutators.Default.OpSubMutator':do_mutate('Elixir.Darwin.DefaultMutators.AbstractCodeTests.FullModuleTest.Mod2',
                                                 3,
                                                 _x@1,
                                                 _y@1)).
                 """
  end

  test "module - ignore macros" do
    {form_list, _ctx} =
      mutate_elixir_module(__MODULE__.Mod2, """
        defmacro m(x, y) do
          quote do
            unquote(x) + unquote(y)
          end
        end
      """)

    assert_value Erlang.pprint_forms(form_list, indent: 2) == """
                 -file(\"lib/darwin/ex_to_erl.ex\", 368).
    
                 -module('Elixir.Darwin.DefaultMutators.AbstractCodeTests.FullModuleTest.Mod2').
    
                 -compile([no_auto_import]).
    
                 -export(['MACRO-m'/3, '__info__'/1]).
    
                 -spec '__info__'(attributes | compile | functions |
                      macros | md5 | module | deprecated) -> any().
    
                 '__info__'(module) ->
                     'Elixir.Darwin.DefaultMutators.AbstractCodeTests.FullModuleTest.Mod2';
                 '__info__'(functions) -> [];
                 '__info__'(macros) -> [{m, 2}];
                 '__info__'(Key = attributes) ->
                     erlang:get_module_info('Elixir.Darwin.DefaultMutators.AbstractCodeTests.FullModuleTest.Mod2',
                          Key);
                 '__info__'(Key = compile) ->
                     erlang:get_module_info('Elixir.Darwin.DefaultMutators.AbstractCodeTests.FullModuleTest.Mod2',
                          Key);
                 '__info__'(Key = md5) ->
                     erlang:get_module_info('Elixir.Darwin.DefaultMutators.AbstractCodeTests.FullModuleTest.Mod2',
                          Key);
                 '__info__'(deprecated) -> [].
    
                 'MACRO-m'(_@CALLER, _x@1, _y@1) ->
                     {'+',
                      [{context,
                        'Elixir.Darwin.DefaultMutators.AbstractCodeTests.FullModuleTest.Mod2'},
                       {import, 'Elixir.Kernel'}],
                      [_x@1, _y@1]}.
                 """
  end
end
