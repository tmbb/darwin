-file("test/common/helpers.ex", 37).

-module('Elixir.Darwin.DefaultMutators.AbstractCodeTests.GuardsRewriterTest.Example2_Module_Mutated').

-compile([no_auto_import]).

-export(['__info__'/1, f/2]).

-spec '__info__'(attributes | compile | functions |
                 macros | md5 | module | deprecated) -> any().

'__info__'(module) ->
    'Elixir.Darwin.DefaultMutators.AbstractCodeTests.GuardsRewriterTest.Example2_Module_Mutated';
'__info__'(functions) -> [{f, 2}];
'__info__'(macros) -> [];
'__info__'(Key = attributes) ->
    erlang:get_module_info('Elixir.Darwin.DefaultMutators.AbstractCodeTests.GuardsRewriterTest.Example2_Module_Mutated',
                           Key);
'__info__'(Key = compile) ->
    erlang:get_module_info('Elixir.Darwin.DefaultMutators.AbstractCodeTests.GuardsRewriterTest.Example2_Module_Mutated',
                           Key);
'__info__'(Key = md5) ->
    erlang:get_module_info('Elixir.Darwin.DefaultMutators.AbstractCodeTests.GuardsRewriterTest.Example2_Module_Mutated',
                           Key);
'__info__'(deprecated) -> [].

f(__@1, __@2) -> random_integer(__@1, __@2).

random_integer(X1_@darwin, X2_@darwin) ->
    Clauses_@darwin = [fun ({__@1, __@1}) ->
                               case try erlang:is_integer(__@1) catch
                                      error:_ -> false
                                    end
                                   of
                                 true -> {ok, begin __@1 end};
                                 false -> error
                               end;
                           (_) -> error
                       end,
                       fun ({__@1, __@2}) ->
                               case try
                                      'Elixir.Darwin.Mutators.Default.OpLessThanMutator':darwin_was_here('Elixir.Darwin.DefaultMutators.AbstractCodeTests.GuardsRewriterTest.Example2_Module_Mutated',
                                                                                                         0,
                                                                                                         __@2,
                                                                                                         __@1)
                                    catch
                                      error:_ -> false
                                    end
                                   of
                                 true ->
                                     {ok, begin random_integer(__@2, __@1) end};
                                 false -> error
                               end;
                           (_) -> error
                       end,
                       fun ({__@1, __@2}) ->
                               {ok,
                                begin
                                  __@1 + rand:uniform(__@2 - __@1 + 1) - 1
                                end};
                           (_) -> error
                       end],
    'Elixir.Darwin.Mutator.GuardRewriter':execute_transformed_clauses(Clauses_@darwin,
                                                                      {X1_@darwin,
                                                                       X2_@darwin}).
