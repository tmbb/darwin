% This is simpler than the version for clauses with guards, which are more complex.
% The argument only needs to match the pattern, which can be done with a simple
% case statement.
% Compare the code in "clause_with_guards.hrl", which is much more complex.

% Matching the patterns in the function head cuases % the variables to be shadowed,
% which is the correct behaviour for case statements.
fun (PatternTuple) ->
      % If the patterns did match, this means we must try to run this clause.
      % The return value will be a `{ok, Body}` so that we can see simulataneously
      % from the "outside" of the function/case statement whether the clause matched
      % and what the result was (the `Body`) in a single step.
      {ok, Body};

    % If the pattern doesn't match, return an error so that we can try
    % to match the next clauses
    (_) ->
      error
end.