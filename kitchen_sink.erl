-file("lib/mod.ex", 1).

-module('Elixir.Darwin.KitchenSink').

-compile([no_auto_import]).

-export(['__info__'/1, arithmetic_operators/2,
         bitwise_operators/2, comparison_operators/2,
         strict_boolean_operators/2]).

-spec '__info__'(attributes | compile | functions |
                 macros | md5 | module | deprecated) -> any().

'__info__'(module) -> 'Elixir.Darwin.KitchenSink';
'__info__'(functions) ->
    [{arithmetic_operators, 2}, {bitwise_operators, 2},
     {comparison_operators, 2},
     {strict_boolean_operators, 2}];
'__info__'(macros) -> [];
'__info__'(Key = attributes) ->
    erlang:get_module_info('Elixir.Darwin.KitchenSink',
                           Key);
'__info__'(Key = compile) ->
    erlang:get_module_info('Elixir.Darwin.KitchenSink',
                           Key);
'__info__'(Key = md5) ->
    erlang:get_module_info('Elixir.Darwin.KitchenSink',
                           Key);
'__info__'(deprecated) -> [].

arithmetic_operators(_a@1, _b@1) ->
    __x@1 = fun (_x@1) -> _x@1 + 1 end,
    _ = _a@1 + _b@1,
    _ = _a@1 - _b@1,
    _ = _a@1 * _b@1,
    _ = _a@1 / _b@1.

bitwise_operators(_a@1, _b@1) -> _ = _a@1 bsr _b@1.

comparison_operators(_a@1, _b@1) ->
    _ = _a@1 < _b@1,
    _ = _a@1 =< _b@1,
    _ = _a@1 == _b@1,
    _ = _a@1 =:= _b@1,
    _ = _a@1 =/= _b@1,
    _ = _a@1 /= _b@1,
    _ = _a@1 > _b@1,
    _ = _a@1 >= _b@1.

strict_boolean_operators(_a@1, _b@1) ->
    _ = case _a@1 of
          false -> false;
          true -> _b@1;
          __@1 -> erlang:error({badbool, 'and', __@1})
        end,
    _ = case _a@1 of
          false -> _b@1;
          true -> true;
          __@2 -> erlang:error({badbool, 'or', __@2})
        end,
    _ = not _a@1.
