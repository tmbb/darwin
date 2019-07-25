def(
  mutate({:op, line, :+, left, right} = _abstract_code, %Darwin.Mutator.Context{} = ctx, mutators)
) do
  alias(Darwin.Mutator.Helpers.DefMutator)
  alias(Darwin.ErlUtils.AbstractCode)
  alias(Darwin.Mutator.Context)
  {mutated_left, ctx} = DefMutator.apply_mutators(mutators, left, ctx)
  {mutated_right, ctx} = DefMutator.apply_mutators(mutators, right, ctx)
  count = ctx.count()
  module = ctx.module()

  ast =
    AbstractCode.call_mfa(
      {Darwin.Mutators.Default.OpAddMutator, :__do_mutate__,
       [
         AbstractCode.encode_atom(module),
         AbstractCode.encode_integer(count, line),
         mutated_left,
         mutated_right
       ]},
      line
    )

  new_ctx =
    Context.add_mutations(ctx, [
      %{
        __struct__: Darwin.Mutation,
        message: "replace '+' by '-' (e.g. `a + b` -> `b - a`)",
        type: :+
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '+' by '*' (e.g. `a + b` -> `b * a`)",
        type: :+
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '+' by '/' (e.g. `a + b` -> `b / a`)",
        type: :+
      },
      %{
        __struct__: Darwin.Mutation,
        message: "'+' operator: delete right argument (e.g. `a + b` -> `a`)",
        type: :+
      },
      %{
        __struct__: Darwin.Mutation,
        message: "'+' operator: delete left argument (e.g. `a + b` -> `b`)",
        type: :+
      }
    ])

  {:ok, {ast, new_ctx}}
end

def(mutate(_ast, _ctx, _mutators)) do
  :error
end

@doc false
def(__do_mutate__(module, line, start, left, right)) do
  {active_module, active_mutation_nr} = Darwin.ActiveMutation.get()
  corrected_index = active_mutation_nr - start

  case(module == active_module) do
    true ->
      corrected_index
      |> case do
        0 ->
          apply(fn a, b -> (&Kernel.-/2).(b, a) end, [left, right])

        1 ->
          apply(fn a, b -> (&Kernel.*/2).(b, a) end, [left, right])

        2 ->
          apply(fn a, b -> (&Kernel.//2).(b, a) end, [left, right])

        3 ->
          apply(fn a, _b -> a end, [left, right])

        4 ->
          apply(fn _a, b -> b end, [left, right])

        _ ->
          apply(&Kernel.+/2, [left, right])
      end

    false ->
      apply(&Kernel.+/2, [left, right])
  end
end

def(
  mutate({:op, line, :-, left, right} = _abstract_code, %Darwin.Mutator.Context{} = ctx, mutators)
) do
  alias(Darwin.Mutator.Helpers.DefMutator)
  alias(Darwin.ErlUtils.AbstractCode)
  alias(Darwin.Mutator.Context)
  {mutated_left, ctx} = DefMutator.apply_mutators(mutators, left, ctx)
  {mutated_right, ctx} = DefMutator.apply_mutators(mutators, right, ctx)
  count = ctx.count()
  module = ctx.module()

  ast =
    AbstractCode.call_mfa(
      {Darwin.Mutators.Default.OpSubMutator, :__do_mutate__,
       [
         AbstractCode.encode_atom(module),
         AbstractCode.encode_integer(count, line),
         mutated_left,
         mutated_right
       ]},
      line
    )

  new_ctx =
    Context.add_mutations(ctx, [
      %{
        __struct__: Darwin.Mutation,
        message: "replace '-' by '+' (e.g. `a - b` -> `b + a`)",
        type: :-
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '-' by '*' (e.g. `a - b` -> `b * a`)",
        type: :-
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '-' by '/' (e.g. `a - b` -> `b / a`)",
        type: :-
      },
      %{
        __struct__: Darwin.Mutation,
        message: "'-' operator: delete right argument (e.g. `a - b` -> `a`)",
        type: :-
      },
      %{
        __struct__: Darwin.Mutation,
        message: "'-' operator: delete left argument (e.g. `a - b` -> `b`)",
        type: :-
      }
    ])

  {:ok, {ast, new_ctx}}
end

def(mutate(_ast, _ctx, _mutators)) do
  :error
end

@doc false
def(__do_mutate__(module, line, start, left, right)) do
  {active_module, active_mutation_nr} = Darwin.ActiveMutation.get()
  corrected_index = active_mutation_nr - start

  case(module == active_module) do
    true ->
      corrected_index
      |> case do
        0 ->
          apply(fn a, b -> (&Kernel.+/2).(b, a) end, [left, right])

        1 ->
          apply(fn a, b -> (&Kernel.*/2).(b, a) end, [left, right])

        2 ->
          apply(fn a, b -> (&Kernel.//2).(b, a) end, [left, right])

        3 ->
          apply(fn a, _b -> a end, [left, right])

        4 ->
          apply(fn _a, b -> b end, [left, right])

        _ ->
          apply(&Kernel.-/2, [left, right])
      end

    false ->
      apply(&Kernel.-/2, [left, right])
  end
end

def(
  mutate({:op, line, :*, left, right} = _abstract_code, %Darwin.Mutator.Context{} = ctx, mutators)
) do
  alias(Darwin.Mutator.Helpers.DefMutator)
  alias(Darwin.ErlUtils.AbstractCode)
  alias(Darwin.Mutator.Context)
  {mutated_left, ctx} = DefMutator.apply_mutators(mutators, left, ctx)
  {mutated_right, ctx} = DefMutator.apply_mutators(mutators, right, ctx)
  count = ctx.count()
  module = ctx.module()

  ast =
    AbstractCode.call_mfa(
      {Darwin.Mutators.Default.OpMulMutator, :__do_mutate__,
       [
         AbstractCode.encode_atom(module),
         AbstractCode.encode_integer(count, line),
         mutated_left,
         mutated_right
       ]},
      line
    )

  new_ctx =
    Context.add_mutations(ctx, [
      %{
        __struct__: Darwin.Mutation,
        message: "replace '*' by '+' (e.g. `a * b` -> `b + a`)",
        type: :*
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '*' by '-' (e.g. `a * b` -> `b - a`)",
        type: :*
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '*' by '/' (e.g. `a * b` -> `b / a`)",
        type: :*
      },
      %{
        __struct__: Darwin.Mutation,
        message: "'*' operator: delete right argument (e.g. `a * b` -> `a`)",
        type: :*
      },
      %{
        __struct__: Darwin.Mutation,
        message: "'*' operator: delete left argument (e.g. `a * b` -> `b`)",
        type: :*
      }
    ])

  {:ok, {ast, new_ctx}}
end

def(mutate(_ast, _ctx, _mutators)) do
  :error
end

@doc false
def(__do_mutate__(module, line, start, left, right)) do
  {active_module, active_mutation_nr} = Darwin.ActiveMutation.get()
  corrected_index = active_mutation_nr - start

  case(module == active_module) do
    true ->
      corrected_index
      |> case do
        0 ->
          apply(fn a, b -> (&Kernel.+/2).(b, a) end, [left, right])

        1 ->
          apply(fn a, b -> (&Kernel.-/2).(b, a) end, [left, right])

        2 ->
          apply(fn a, b -> (&Kernel.//2).(b, a) end, [left, right])

        3 ->
          apply(fn a, _b -> a end, [left, right])

        4 ->
          apply(fn _a, b -> b end, [left, right])

        _ ->
          apply(&Kernel.*/2, [left, right])
      end

    false ->
      apply(&Kernel.*/2, [left, right])
  end
end

def(
  mutate({:op, line, :/, left, right} = _abstract_code, %Darwin.Mutator.Context{} = ctx, mutators)
) do
  alias(Darwin.Mutator.Helpers.DefMutator)
  alias(Darwin.ErlUtils.AbstractCode)
  alias(Darwin.Mutator.Context)
  {mutated_left, ctx} = DefMutator.apply_mutators(mutators, left, ctx)
  {mutated_right, ctx} = DefMutator.apply_mutators(mutators, right, ctx)
  count = ctx.count()
  module = ctx.module()

  ast =
    AbstractCode.call_mfa(
      {Darwin.Mutators.Default.OpDivMutator, :__do_mutate__,
       [
         AbstractCode.encode_atom(module),
         AbstractCode.encode_integer(count, line),
         mutated_left,
         mutated_right
       ]},
      line
    )

  new_ctx =
    Context.add_mutations(ctx, [
      %{
        __struct__: Darwin.Mutation,
        message: "replace '/' by '+' (e.g. `a / b` -> `b + a`)",
        type: :/
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '/' by '-' (e.g. `a / b` -> `b - a`)",
        type: :/
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '/' by '*' (e.g. `a / b` -> `b * a`)",
        type: :/
      },
      %{
        __struct__: Darwin.Mutation,
        message: "'/' operator: delete right argument (e.g. `a / b` -> `a`)",
        type: :/
      },
      %{
        __struct__: Darwin.Mutation,
        message: "'/' operator: delete left argument (e.g. `a / b` -> `b`)",
        type: :/
      }
    ])

  {:ok, {ast, new_ctx}}
end

def(mutate(_ast, _ctx, _mutators)) do
  :error
end

@doc false
def(__do_mutate__(module, line, start, left, right)) do
  {active_module, active_mutation_nr} = Darwin.ActiveMutation.get()
  corrected_index = active_mutation_nr - start

  case(module == active_module) do
    true ->
      corrected_index
      |> case do
        0 ->
          apply(fn a, b -> (&Kernel.+/2).(b, a) end, [left, right])

        1 ->
          apply(fn a, b -> (&Kernel.-/2).(b, a) end, [left, right])

        2 ->
          apply(fn a, b -> (&Kernel.*/2).(b, a) end, [left, right])

        3 ->
          apply(fn a, _b -> a end, [left, right])

        4 ->
          apply(fn _a, b -> b end, [left, right])

        _ ->
          apply(&Kernel.//2, [left, right])
      end

    false ->
      apply(&Kernel.//2, [left, right])
  end
end

def(
  mutate({:op, line, :<, left, right} = _abstract_code, %Darwin.Mutator.Context{} = ctx, mutators)
) do
  alias(Darwin.Mutator.Helpers.DefMutator)
  alias(Darwin.ErlUtils.AbstractCode)
  alias(Darwin.Mutator.Context)
  {mutated_left, ctx} = DefMutator.apply_mutators(mutators, left, ctx)
  {mutated_right, ctx} = DefMutator.apply_mutators(mutators, right, ctx)
  count = ctx.count()
  module = ctx.module()

  ast =
    AbstractCode.call_mfa(
      {Darwin.Mutators.Default.OpLessThanMutator, :__do_mutate__,
       [
         AbstractCode.encode_atom(module),
         AbstractCode.encode_integer(count, line),
         mutated_left,
         mutated_right
       ]},
      line
    )

  new_ctx =
    Context.add_mutations(ctx, [
      %{
        __struct__: Darwin.Mutation,
        message: "replace '<' by '<=' (e.g. `a < b` -> `b <= a`)",
        type: :<
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '<' by '==' (e.g. `a < b` -> `b == a`)",
        type: :<
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '<' by '!=' (e.g. `a < b` -> `b != a`)",
        type: :<
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '<' by '>=' (e.g. `a < b` -> `b >= a`)",
        type: :<
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '<' by '>' (e.g. `a < b` -> `b > a`)",
        type: :<
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '<' by comparison that always fails (e.g. `a < b` -> `false`)",
        type: :<
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '<' by comparison that always succeeds (e.g. `a < b` -> `true`)",
        type: :<
      }
    ])

  {:ok, {ast, new_ctx}}
end

def(mutate(_ast, _ctx, _mutators)) do
  :error
end

@doc false
def(__do_mutate__(module, line, start, left, right)) do
  {active_module, active_mutation_nr} = Darwin.ActiveMutation.get()
  corrected_index = active_mutation_nr - start

  case(module == active_module) do
    true ->
      corrected_index
      |> case do
        0 ->
          apply(fn a, b -> (&Kernel.<=/2).(b, a) end, [left, right])

        1 ->
          apply(fn a, b -> (&Kernel.==/2).(b, a) end, [left, right])

        2 ->
          apply(fn a, b -> (&Kernel.!=/2).(b, a) end, [left, right])

        3 ->
          apply(fn a, b -> (&Kernel.>=/2).(b, a) end, [left, right])

        4 ->
          apply(fn a, b -> (&Kernel.>/2).(b, a) end, [left, right])

        5 ->
          apply(fn _a, _b -> false end, [left, right])

        6 ->
          apply(fn _a, _b -> true end, [left, right])

        _ ->
          apply(&Kernel.</2, [left, right])
      end

    false ->
      apply(&Kernel.</2, [left, right])
  end
end

def(
  mutate(
    {:op, line, :<=, left, right} = _abstract_code,
    %Darwin.Mutator.Context{} = ctx,
    mutators
  )
) do
  alias(Darwin.Mutator.Helpers.DefMutator)
  alias(Darwin.ErlUtils.AbstractCode)
  alias(Darwin.Mutator.Context)
  {mutated_left, ctx} = DefMutator.apply_mutators(mutators, left, ctx)
  {mutated_right, ctx} = DefMutator.apply_mutators(mutators, right, ctx)
  count = ctx.count()
  module = ctx.module()

  ast =
    AbstractCode.call_mfa(
      {Darwin.Mutators.Default.OpLessThanOrEqualToMutator, :__do_mutate__,
       [
         AbstractCode.encode_atom(module),
         AbstractCode.encode_integer(count, line),
         mutated_left,
         mutated_right
       ]},
      line
    )

  new_ctx =
    Context.add_mutations(ctx, [
      %{
        __struct__: Darwin.Mutation,
        message: "replace '<=' by '<' (e.g. `a <= b` -> `b < a`)",
        type: :<=
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '<=' by '==' (e.g. `a <= b` -> `b == a`)",
        type: :<=
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '<=' by '!=' (e.g. `a <= b` -> `b != a`)",
        type: :<=
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '<=' by '>=' (e.g. `a <= b` -> `b >= a`)",
        type: :<=
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '<=' by '>' (e.g. `a <= b` -> `b > a`)",
        type: :<=
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '<=' by comparison that always fails (e.g. `a <= b` -> `false`)",
        type: :<=
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '<=' by comparison that always succeeds (e.g. `a <= b` -> `true`)",
        type: :<=
      }
    ])

  {:ok, {ast, new_ctx}}
end

def(mutate(_ast, _ctx, _mutators)) do
  :error
end

@doc false
def(__do_mutate__(module, line, start, left, right)) do
  {active_module, active_mutation_nr} = Darwin.ActiveMutation.get()
  corrected_index = active_mutation_nr - start

  case(module == active_module) do
    true ->
      corrected_index
      |> case do
        0 ->
          apply(fn a, b -> (&Kernel.</2).(b, a) end, [left, right])

        1 ->
          apply(fn a, b -> (&Kernel.==/2).(b, a) end, [left, right])

        2 ->
          apply(fn a, b -> (&Kernel.!=/2).(b, a) end, [left, right])

        3 ->
          apply(fn a, b -> (&Kernel.>=/2).(b, a) end, [left, right])

        4 ->
          apply(fn a, b -> (&Kernel.>/2).(b, a) end, [left, right])

        5 ->
          apply(fn _a, _b -> false end, [left, right])

        6 ->
          apply(fn _a, _b -> true end, [left, right])

        _ ->
          apply(&Kernel.<=/2, [left, right])
      end

    false ->
      apply(&Kernel.<=/2, [left, right])
  end
end

def(
  mutate(
    {:op, line, :==, left, right} = _abstract_code,
    %Darwin.Mutator.Context{} = ctx,
    mutators
  )
) do
  alias(Darwin.Mutator.Helpers.DefMutator)
  alias(Darwin.ErlUtils.AbstractCode)
  alias(Darwin.Mutator.Context)
  {mutated_left, ctx} = DefMutator.apply_mutators(mutators, left, ctx)
  {mutated_right, ctx} = DefMutator.apply_mutators(mutators, right, ctx)
  count = ctx.count()
  module = ctx.module()

  ast =
    AbstractCode.call_mfa(
      {Darwin.Mutators.Default.OpEqualToMutator, :__do_mutate__,
       [
         AbstractCode.encode_atom(module),
         AbstractCode.encode_integer(count, line),
         mutated_left,
         mutated_right
       ]},
      line
    )

  new_ctx =
    Context.add_mutations(ctx, [
      %{
        __struct__: Darwin.Mutation,
        message: "replace '==' by '<' (e.g. `a == b` -> `b < a`)",
        type: :==
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '==' by '<=' (e.g. `a == b` -> `b <= a`)",
        type: :==
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '==' by '!=' (e.g. `a == b` -> `b != a`)",
        type: :==
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '==' by '>=' (e.g. `a == b` -> `b >= a`)",
        type: :==
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '==' by '>' (e.g. `a == b` -> `b > a`)",
        type: :==
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '==' by comparison that always fails (e.g. `a == b` -> `false`)",
        type: :==
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '==' by comparison that always succeeds (e.g. `a == b` -> `true`)",
        type: :==
      }
    ])

  {:ok, {ast, new_ctx}}
end

def(mutate(_ast, _ctx, _mutators)) do
  :error
end

@doc false
def(__do_mutate__(module, line, start, left, right)) do
  {active_module, active_mutation_nr} = Darwin.ActiveMutation.get()
  corrected_index = active_mutation_nr - start

  case(module == active_module) do
    true ->
      corrected_index
      |> case do
        0 ->
          apply(fn a, b -> (&Kernel.</2).(b, a) end, [left, right])

        1 ->
          apply(fn a, b -> (&Kernel.<=/2).(b, a) end, [left, right])

        2 ->
          apply(fn a, b -> (&Kernel.!=/2).(b, a) end, [left, right])

        3 ->
          apply(fn a, b -> (&Kernel.>=/2).(b, a) end, [left, right])

        4 ->
          apply(fn a, b -> (&Kernel.>/2).(b, a) end, [left, right])

        5 ->
          apply(fn _a, _b -> false end, [left, right])

        6 ->
          apply(fn _a, _b -> true end, [left, right])

        _ ->
          apply(&Kernel.==/2, [left, right])
      end

    false ->
      apply(&Kernel.==/2, [left, right])
  end
end

def(
  mutate(
    {:op, line, :!=, left, right} = _abstract_code,
    %Darwin.Mutator.Context{} = ctx,
    mutators
  )
) do
  alias(Darwin.Mutator.Helpers.DefMutator)
  alias(Darwin.ErlUtils.AbstractCode)
  alias(Darwin.Mutator.Context)
  {mutated_left, ctx} = DefMutator.apply_mutators(mutators, left, ctx)
  {mutated_right, ctx} = DefMutator.apply_mutators(mutators, right, ctx)
  count = ctx.count()
  module = ctx.module()

  ast =
    AbstractCode.call_mfa(
      {Darwin.Mutators.Default.OpNotEqualToMutator, :__do_mutate__,
       [
         AbstractCode.encode_atom(module),
         AbstractCode.encode_integer(count, line),
         mutated_left,
         mutated_right
       ]},
      line
    )

  new_ctx =
    Context.add_mutations(ctx, [
      %{
        __struct__: Darwin.Mutation,
        message: "replace '!=' by '<' (e.g. `a != b` -> `b < a`)",
        type: :!=
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '!=' by '<=' (e.g. `a != b` -> `b <= a`)",
        type: :!=
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '!=' by '==' (e.g. `a != b` -> `b == a`)",
        type: :!=
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '!=' by '>=' (e.g. `a != b` -> `b >= a`)",
        type: :!=
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '!=' by '>' (e.g. `a != b` -> `b > a`)",
        type: :!=
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '!=' by comparison that always fails (e.g. `a != b` -> `false`)",
        type: :!=
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '!=' by comparison that always succeeds (e.g. `a != b` -> `true`)",
        type: :!=
      }
    ])

  {:ok, {ast, new_ctx}}
end

def(mutate(_ast, _ctx, _mutators)) do
  :error
end

@doc false
def(__do_mutate__(module, line, start, left, right)) do
  {active_module, active_mutation_nr} = Darwin.ActiveMutation.get()
  corrected_index = active_mutation_nr - start

  case(module == active_module) do
    true ->
      corrected_index
      |> case do
        0 ->
          apply(fn a, b -> (&Kernel.</2).(b, a) end, [left, right])

        1 ->
          apply(fn a, b -> (&Kernel.<=/2).(b, a) end, [left, right])

        2 ->
          apply(fn a, b -> (&Kernel.==/2).(b, a) end, [left, right])

        3 ->
          apply(fn a, b -> (&Kernel.>=/2).(b, a) end, [left, right])

        4 ->
          apply(fn a, b -> (&Kernel.>/2).(b, a) end, [left, right])

        5 ->
          apply(fn _a, _b -> false end, [left, right])

        6 ->
          apply(fn _a, _b -> true end, [left, right])

        _ ->
          apply(&Kernel.!=/2, [left, right])
      end

    false ->
      apply(&Kernel.!=/2, [left, right])
  end
end

def(
  mutate(
    {:op, line, :>=, left, right} = _abstract_code,
    %Darwin.Mutator.Context{} = ctx,
    mutators
  )
) do
  alias(Darwin.Mutator.Helpers.DefMutator)
  alias(Darwin.ErlUtils.AbstractCode)
  alias(Darwin.Mutator.Context)
  {mutated_left, ctx} = DefMutator.apply_mutators(mutators, left, ctx)
  {mutated_right, ctx} = DefMutator.apply_mutators(mutators, right, ctx)
  count = ctx.count()
  module = ctx.module()

  ast =
    AbstractCode.call_mfa(
      {Darwin.Mutators.Default.OpGreaterThanOrEqualToMutator, :__do_mutate__,
       [
         AbstractCode.encode_atom(module),
         AbstractCode.encode_integer(count, line),
         mutated_left,
         mutated_right
       ]},
      line
    )

  new_ctx =
    Context.add_mutations(ctx, [
      %{
        __struct__: Darwin.Mutation,
        message: "replace '>=' by '<' (e.g. `a >= b` -> `b < a`)",
        type: :>=
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '>=' by '<=' (e.g. `a >= b` -> `b <= a`)",
        type: :>=
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '>=' by '==' (e.g. `a >= b` -> `b == a`)",
        type: :>=
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '>=' by '!=' (e.g. `a >= b` -> `b != a`)",
        type: :>=
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '>=' by '>' (e.g. `a >= b` -> `b > a`)",
        type: :>=
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '>=' by comparison that always fails (e.g. `a >= b` -> `false`)",
        type: :>=
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '>=' by comparison that always succeeds (e.g. `a >= b` -> `true`)",
        type: :>=
      }
    ])

  {:ok, {ast, new_ctx}}
end

def(mutate(_ast, _ctx, _mutators)) do
  :error
end

@doc false
def(__do_mutate__(module, line, start, left, right)) do
  {active_module, active_mutation_nr} = Darwin.ActiveMutation.get()
  corrected_index = active_mutation_nr - start

  case(module == active_module) do
    true ->
      corrected_index
      |> case do
        0 ->
          apply(fn a, b -> (&Kernel.</2).(b, a) end, [left, right])

        1 ->
          apply(fn a, b -> (&Kernel.<=/2).(b, a) end, [left, right])

        2 ->
          apply(fn a, b -> (&Kernel.==/2).(b, a) end, [left, right])

        3 ->
          apply(fn a, b -> (&Kernel.!=/2).(b, a) end, [left, right])

        4 ->
          apply(fn a, b -> (&Kernel.>/2).(b, a) end, [left, right])

        5 ->
          apply(fn _a, _b -> false end, [left, right])

        6 ->
          apply(fn _a, _b -> true end, [left, right])

        _ ->
          apply(&Kernel.>=/2, [left, right])
      end

    false ->
      apply(&Kernel.>=/2, [left, right])
  end
end

def(
  mutate({:op, line, :>, left, right} = _abstract_code, %Darwin.Mutator.Context{} = ctx, mutators)
) do
  alias(Darwin.Mutator.Helpers.DefMutator)
  alias(Darwin.ErlUtils.AbstractCode)
  alias(Darwin.Mutator.Context)
  {mutated_left, ctx} = DefMutator.apply_mutators(mutators, left, ctx)
  {mutated_right, ctx} = DefMutator.apply_mutators(mutators, right, ctx)
  count = ctx.count()
  module = ctx.module()

  ast =
    AbstractCode.call_mfa(
      {Darwin.Mutators.Default.OpGreaterThanMutator, :__do_mutate__,
       [
         AbstractCode.encode_atom(module),
         AbstractCode.encode_integer(count, line),
         mutated_left,
         mutated_right
       ]},
      line
    )

  new_ctx =
    Context.add_mutations(ctx, [
      %{
        __struct__: Darwin.Mutation,
        message: "replace '>' by '<' (e.g. `a > b` -> `b < a`)",
        type: :>
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '>' by '<=' (e.g. `a > b` -> `b <= a`)",
        type: :>
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '>' by '==' (e.g. `a > b` -> `b == a`)",
        type: :>
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '>' by '!=' (e.g. `a > b` -> `b != a`)",
        type: :>
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '>' by '>=' (e.g. `a > b` -> `b >= a`)",
        type: :>
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '>' by comparison that always fails (e.g. `a > b` -> `false`)",
        type: :>
      },
      %{
        __struct__: Darwin.Mutation,
        message: "replace '>' by comparison that always succeeds (e.g. `a > b` -> `true`)",
        type: :>
      }
    ])

  {:ok, {ast, new_ctx}}
end

def(mutate(_ast, _ctx, _mutators)) do
  :error
end

@doc false
def(__do_mutate__(module, line, start, left, right)) do
  {active_module, active_mutation_nr} = Darwin.ActiveMutation.get()
  corrected_index = active_mutation_nr - start

  case(module == active_module) do
    true ->
      corrected_index
      |> case do
        0 ->
          apply(fn a, b -> (&Kernel.</2).(b, a) end, [left, right])

        1 ->
          apply(fn a, b -> (&Kernel.<=/2).(b, a) end, [left, right])

        2 ->
          apply(fn a, b -> (&Kernel.==/2).(b, a) end, [left, right])

        3 ->
          apply(fn a, b -> (&Kernel.!=/2).(b, a) end, [left, right])

        4 ->
          apply(fn a, b -> (&Kernel.>=/2).(b, a) end, [left, right])

        5 ->
          apply(fn _a, _b -> false end, [left, right])

        6 ->
          apply(fn _a, _b -> true end, [left, right])

        _ ->
          apply(&Kernel.>/2, [left, right])
      end

    false ->
      apply(&Kernel.>/2, [left, right])
  end
end