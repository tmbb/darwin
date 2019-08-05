-file("lib/mod.ex", 1).

-module('Elixir.Darwin.KitchenSink').

-compile([no_auto_import]).

-export(['__info__'/1, arithmetic_operators/2,
         bitwise_operators/2, comparison_operators/2]).

-spec '__info__'(attributes | compile | functions |
                 macros | md5 | module | deprecated) -> any().

'__info__'(module) -> 'Elixir.Darwin.KitchenSink';
'__info__'(functions) ->
    [{arithmetic_operators, 2}, {bitwise_operators, 2},
     {comparison_operators, 2}];
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
