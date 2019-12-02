[
  {:fun, 1,
   {:clauses,
    [
      {:clause, 1,
       [
         {:tuple, 0,
          [
            {:match, 2, {:tuple, 0, [{:var, 2, :_x@1}, {:var, 2, :__y@1}]}, {:var, 2, :__a@1}},
            {:var, 2, :_b@1}
          ]}
       ], [],
       [
         {:try, 2,
          [
            {:case, 2,
             {:op, 2, :andalso,
              {:call, 2, {:remote, 2, {:atom, 0, :erlang}, {:atom, 2, :is_integer}},
               [{:var, 2, :_x@1}]},
              {:call, 2, {:remote, 2, {:atom, 0, :erlang}, {:atom, 2, :is_integer}},
               [{:var, 2, :_b@1}]}},
             [
               {:clause, 3, [{:atom, 3, true}], [],
                [
                  {:tuple, 3,
                   [
                     {:atom, 3, :ok},
                     {:block, 0, [{:op, 2, :+, {:var, 2, :_x@1}, {:var, 2, :_b@1}}]}
                   ]}
                ]},
               {:clause, 4, [{:atom, 4, false}], [], [{:atom, 4, :error}]}
             ]}
          ], [],
          [
            {:clause, 7, [{:tuple, 7, [{:atom, 7, :error}, {:var, 7, :_}, {:var, 7, :_}]}], [],
             [{:atom, 7, :error}]}
          ], []}
       ]},
      {:clause, 9, [{:var, 9, :_}], [], [{:atom, 9, :error}]}
    ]}}
]
