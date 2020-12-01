% The argument of this function will match everything
% We will refine the match later.
% We can't add the pattern here because that would cause
% the variables to be shadowed, which is incorrect behaviour
% for case statements.
fun (DummyArgument) ->
  case DummyArgument of
    % Note that variables occuring inside the `PatternTuple`
    % will match variables with the same name outside the function.
    % This is the correct behaviour!
    PatternTuple ->
      case (try
              % Try to match the guards
              % The `GuardsMatch` variable will be replaced with an
              % expression which is equivalent to matching the guards in order
              % (it returns `true` if the guards match and `false` if they don't.
              GuardsMatch
            catch
              % If any of the guards causes an error
              % it's the same as if they didn't match.
              % The function shouldn't raise an error in this case.
              error:_ -> false
            end) of
        % If the guards did match, this means we must try to run this clause.
        % The return value will be a `{ok, Body}` so that we can see simulataneously
        % from the "outside" of the function/case statement whether the clause matched
        % (both the pattern and the guards) and what the result was (the `Body`) in
        % a single step.
        true -> {ok, Body};
        % If the pattern and guards didn't match, we're in the wrong clause.
        % Return an `error` atom so that the outside code can tell this clause
        % has failed to match
        false -> error
      end;

    % If the argument doesn't match tha pattern, then return an `error` too.
    % We're in the wrong clause.
    _ -> error
  end
end.