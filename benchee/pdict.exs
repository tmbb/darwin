Benchee.run(%{
  "key doesn't exist (Process)" => {
    fn _input -> Process.get(:"$darwin.active_mutation", nil) end,
    before_scenario: fn _input -> Process.put(:"$darwin.active_mutation", {MyModule, 65, 1}) end,
    after_scenario: fn _input -> Process.delete(:"$darwin.active_mutation") end
  },
  "key doesn't exist (BIF)" => {
    fn _input -> :erlang.get(:"$darwin.active_mutation") end,
    before_scenario: fn _input -> Process.put(:"$darwin.active_mutation", {MyModule, 65, 1}) end,
    after_scenario: fn _input -> Process.delete(:"$darwin.active_mutation") end
  },
  "key exists (Process)" => fn -> Process.get(:"$darwin.active_mutation", nil) end,
  "key exists (BIF)" => fn -> :erlang.get(:"$darwin.active_mutation") end
})
