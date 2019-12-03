-file("test/common/helpers.ex", 37).

-module('Elixir.Darwin.DefaultMutators.AbstractCodeTests.GuardsRewriterTest.Example1_Module_Mutated').

-compile([no_auto_import]).

-export(['__info__'/1, f/1]).

-spec '__info__'(attributes | compile | functions |
                 macros | md5 | module | deprecated) -> any().

'__info__'(module) ->
    'Elixir.Darwin.DefaultMutators.AbstractCodeTests.GuardsRewriterTest.Example1_Module_Mutated';
'__info__'(functions) -> [{f, 1}];
'__info__'(macros) -> [];
'__info__'(Key = attributes) ->
    erlang:get_module_info('Elixir.Darwin.DefaultMutators.AbstractCodeTests.GuardsRewriterTest.Example1_Module_Mutated',
                           Key);
'__info__'(Key = compile) ->
    erlang:get_module_info('Elixir.Darwin.DefaultMutators.AbstractCodeTests.GuardsRewriterTest.Example1_Module_Mutated',
                           Key);
'__info__'(Key = md5) ->
    erlang:get_module_info('Elixir.Darwin.DefaultMutators.AbstractCodeTests.GuardsRewriterTest.Example1_Module_Mutated',
                           Key);
'__info__'(deprecated) -> [].

f(X1_@darwin) ->
    Clauses_@darwin = [fun ({__@1}) ->
                               case try
                                      'Elixir.Darwin.Mutators.Default.OpEqualToMutator':darwin_was_here('Elixir.Darwin.DefaultMutators.AbstractCodeTests.GuardsRewriterTest.Example1_Module_Mutated',
                                                                                                        1,
                                                                                                        __@1,
                                                                                                        'Elixir.Darwin.Mutators.Default.IntegerMutator':darwin_was_here('Elixir.Darwin.DefaultMutators.AbstractCodeTests.GuardsRewriterTest.Example1_Module_Mutated',
                                                                                                                                                                        0,
                                                                                                                                                                        1))
                                    catch
                                      error:_ -> false
                                    end
                                   of
                                 true ->
                                     {ok,
                                      begin
                                        'Elixir.Darwin.Mutators.Default.AtomMutator':darwin_was_here('Elixir.Darwin.DefaultMutators.AbstractCodeTests.GuardsRewriterTest.Example1_Module_Mutated',
                                                                                                     2,
                                                                                                     f1)
                                      end};
                                 false -> error
                               end;
                           (_) -> error
                       end,
                       fun ({__@1}) ->
                               case try
                                      'Elixir.Darwin.Mutators.Default.OpEqualToMutator':darwin_was_here('Elixir.Darwin.DefaultMutators.AbstractCodeTests.GuardsRewriterTest.Example1_Module_Mutated',
                                                                                                        5,
                                                                                                        erlang:element('Elixir.Darwin.Mutators.Default.IntegerMutator':darwin_was_here('Elixir.Darwin.DefaultMutators.AbstractCodeTests.GuardsRewriterTest.Example1_Module_Mutated',
                                                                                                                                                                                       3,
                                                                                                                                                                                       2),
                                                                                                                       __@1),
                                                                                                        'Elixir.Darwin.Mutators.Default.IntegerMutator':darwin_was_here('Elixir.Darwin.DefaultMutators.AbstractCodeTests.GuardsRewriterTest.Example1_Module_Mutated',
                                                                                                                                                                        4,
                                                                                                                                                                        1))
                                    catch
                                      error:_ -> false
                                    end
                                   of
                                 true ->
                                     {ok,
                                      begin
                                        'Elixir.Darwin.Mutators.Default.AtomMutator':darwin_was_here('Elixir.Darwin.DefaultMutators.AbstractCodeTests.GuardsRewriterTest.Example1_Module_Mutated',
                                                                                                     6,
                                                                                                     f2)
                                      end};
                                 false -> error
                               end;
                           (_) -> error
                       end,
                       fun ({{_, 2}}) -> {ok, begin f3 end};
                           (_) -> error
                       end],
    'Elixir.Darwin.Mutator.GuardRewriter':execute_transformed_clauses(Clauses_@darwin,
                                                                      {X1_@darwin}).
