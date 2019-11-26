defmodule Darwin.ExToErl.ModuleQueue do
  @moduledoc false
  # Stolen from somehwere on the internet (I forget where...)
  use GenServer

  @empty_queue :queue.new()
  @typep queue_t :: {[any], [any]}

  @typedoc """
  The `%BlockingQueue` struct is used with the `Collectable` protocol.

  ## Examples

      input = ["Hello", "World"]
      {:ok, pid} = BlockingQueue.start_link(5)
      Enum.into(input, %BlockingQueue{pid: pid})
      BlockingQueue.pop_stream(pid) |> Enum.take(2)  # should return input
  """
  defstruct pid: nil
  @type t :: %__MODULE__{pid: pid()}

  # Can I get this from somewhere?
  @type on_start :: {:ok, pid} | :ignore | {:error, {:already_started, pid} | term}

  @doc """
  Start a queue process with GenServer.start_link/3.

  `n` Is the maximum queue depth.  Pass the atom `:infinity` to start a queue
  with no maximum.  An infinite queue will never block in `push/2` but may
  block in `pop/1`

  `options` Are options as described for `GenServer.start_link/3` and are optional.
  """
  @type maximum_t ::
          pos_integer()
          | :infinity
  @spec start_link([any]) :: on_start
  def start_link(options \\ []) do
    n = Keyword.get(options, :max, 8)
    name = Keyword.get(options, :name, __MODULE__)
    GenServer.start_link(__MODULE__, n, name: name)
  end

  def init(n) do
    modules =
      for i <- 1..n do
        Module.concat(ExToErl.Compiler.Sandboxes, "Sandbox#{i}")
      end

    queue = :queue.from_list(modules)
    {:ok, {n, queue}}
  end

  @typep from_t :: {pid, any}

  @typep state_t ::
           {pos_integer(), queue_t}
           | {pos_integer(), queue_t, :pop, from_t}
           | {pos_integer(), queue_t, :push, from_t, any}

  @typep call_t ::
           {:push, any}
           | :pop

  @typep result_t ::
           {:reply, any, state_t}
           | {:noreply, state_t}

  @spec handle_call(call_t, from_t, state_t) :: result_t

  # start a list of waiting pushers when the first client tries to push to a full queue
  def handle_call({:push, item}, from, {max, queue = {left, right}})
      when length(left) + length(right) >= max do
    {:reply, :block, {max, queue, :push, [{from, item}]}}
  end

  # prepend new waiter to list of waiting pushers when they try to push to a full queue
  def handle_call({:push, item}, from, {max, queue = {left, right}, :push, [next | rest]})
      when length(left) + length(right) >= max do
    {:reply, :block, {max, queue, :push, [{from, item} | [next | rest]]}}
  end

  def handle_call({:push, item}, _, {max, queue}) do
    {:reply, nil, {max, :queue.in(item, queue)}}
  end

  # send item to a single waiting popper
  def handle_call({:push, item}, _, {max, @empty_queue, :pop, [next | []]}) do
    send(elem(next, 0), {:awaken, item})
    {:reply, nil, {max, @empty_queue}}
  end

  # send item to the next in a list of waiting poppers
  def handle_call({:push, item}, _, {max, @empty_queue, :pop, [next | rest]}) do
    send(elem(next, 0), {:awaken, item})
    {:reply, nil, {max, @empty_queue, :pop, rest}}
  end

  # start a list of waiting poppers when the first client tries to pop from the empty queue
  def handle_call(:pop, from, {max, @empty_queue}) do
    {:reply, :block, {max, @empty_queue, :pop, [from]}}
  end

  # prepend new waiter to list of waiting poppers when they try to pop from an empty queue
  def handle_call(:pop, from, {max, @empty_queue, :pop, [next | rest]}) do
    {:reply, :block, {max, @empty_queue, :pop, [from | [next | rest]]}}
  end

  # accept an item pushed by a single waiting pusher
  def handle_call(:pop, _, {max, queue, :push, [{next, item}]}) do
    {{:value, popped_item}, popped_queue} = :queue.out(queue)
    send(elem(next, 0), :awaken)
    final_queue = :queue.in(item, popped_queue)
    {:reply, popped_item, {max, final_queue}}
  end

  # accept an item pushed by the last in a list of waiting pushers (taking last makes this FIFO)
  def handle_call(:pop, _, {max, queue, :push, waiters}) when is_list(waiters) do
    {{:value, popped_item}, popped_queue} = :queue.out(queue)
    {next, item} = List.last(waiters)
    rest = List.delete_at(waiters, -1)
    send(elem(next, 0), :awaken)
    final_queue = :queue.in(item, popped_queue)
    {:reply, popped_item, {max, final_queue, :push, rest}}
  end

  def handle_call(:pop, _, {max, queue}) do
    {{:value, popped_item}, new_queue} = :queue.out(queue)
    {:reply, popped_item, {max, new_queue}}
  end

  @doc """
  Pushes a new item into the queue.  Blocks if the queue is full.

  `pid` is the process ID of the BlockingQueue server.
  `item` is the value to be pushed into the queue.  This can be anything.
  `timeout` (optional) is the timeout value passed to GenServer.call (does not impact how long pop will wait for a message from the queue)
  """
  @spec push(any, integer) :: nil
  def push(item, timeout \\ 5000) do
    case GenServer.call(__MODULE__, {:push, item}, timeout) do
      :block ->
        receive do
          :awaken -> :ok
        end

      _ ->
        nil
    end
  end

  @doc """
  Pops the least recently pushed item from the queue. Blocks if the queue is
  empty until an item is available.

  `pid` is the process ID of the BlockingQueue server.
  `timeout` (optional) is the timeout value passed to GenServer.call (does not impact how long pop will wait for a message from the queue)
  """
  @spec pop(integer) :: any
  def pop(timeout \\ 5000) do
    case GenServer.call(__MODULE__, :pop, timeout) do
      :block ->
        receive do
          {:awaken, data} -> data
        end

      data ->
        data
    end
  end
end
