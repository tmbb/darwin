-file("lib/darwin/ex_to_erl.ex", 0).

-module('Elixir.ModuleA').

-compile([no_auto_import]).

-export(['__info__'/1,
	 g__darwin_guards_no_constants/2]).

-spec '__info__'(attributes | compile | functions |
		 macros | md5 | module | deprecated) -> any().

'__info__'(module) -> 'Elixir.ModuleA';
'__info__'(functions) ->
    [{g__darwin_guards_no_constants, 2}];
'__info__'(macros) -> [];
'__info__'(Key = attributes) ->
    erlang:get_module_info('Elixir.ModuleA', Key);
'__info__'(Key = compile) ->
    erlang:get_module_info('Elixir.ModuleA', Key);
'__info__'(Key = md5) ->
    erlang:get_module_info('Elixir.ModuleA', Key);
'__info__'(deprecated) -> [].

g__darwin_guards_no_constants(_a@1, _b@1) ->
    (erlng:apply(fun (_a@2, {__a@1, __b@1}) ->
			 try case _a@2 == 1 of
			       true -> {ok, g1};
			       false -> error
			     end
			 catch
			   error:_ -> error
			 end;
		     (_, _) -> error
		 end))([_a@1, _b@1]).

f () ->
fun (Arguments) ->
        try (case AllGuardsMatch of
              true -> {ok, Body};
              false -> error
            end)
        catch
          error:_ -> error
        end;
          (_) -> error
      end.