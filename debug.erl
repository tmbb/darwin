f(X1_@darwin, X2_@darwin) ->
    Clauses_@darwin = [fun ({{_x@1, __y@1} = __a@1,
                             _b@1}) ->
                               try case erlang:is_integer(_x@1) andalso
                                          erlang:is_integer(_b@1)
                                       of
                                     true -> {ok, begin _x@1 + _b@1 end};
                                     false -> error
                                   end
                               catch
                                 error:_ -> error
                               end;
                           (_) -> error
                       end,
                       fun ({_a@1, _b@1}) ->
                               try case erlang:is_integer(_b@1) of
                                     true -> {ok, begin _a@1 + _b@1 end};
                                     false -> error
                                   end
                               catch
                                 error:_ -> error
                               end;
                           (_) -> error
                       end],
    'Darwin.Mutator.GuardRewriter':execute_transformed_clauses(Clauses_@darwin,
                                                               {X1_@darwin,
                                                                X2_@darwin}).
