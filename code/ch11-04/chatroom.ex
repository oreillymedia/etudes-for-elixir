defmodule Chatroom do
  use GenServer.Behaviour
  
  # convenience method for startup
  def start_link do
    :gen_server.start_link({:global, __MODULE__}, __MODULE__, [], [])
  end
  
  # callbacks for GenServer.Behaviour
  def init([]) do
    :inets.start()
    {:ok, []}
  end
  
  def handle_call({:login, user, server}, from, state) do
    key = {{user, server}, from}
    IO.puts("Logging in from #{inspect(from)}")
    if List.keymember?(state, key, 0) do
      reply = {:error, "#{user}@#{server} already logged in"}
      new_state = state
    else
      new_state = [key | state]
      reply = {:ok, "#{user}@#{server} logged in."}
    end
    {:reply, reply, new_state}
  end
  
  def handle_call(:logout, from, state) do
    {{user, server}, _pid} = List.keyfind(state, from, 1)
    new_state = List.keydelete(state, from, 1)
    reply = {:ok, "#{user}@#{server} logged out."}
    {:reply, reply, new_state}
  end
  
  def handle_call({:say, text}, from, state) do
    Enum.each(state, fn(item) ->
      {{user, server}, pid} = item
      if pid != from do
        :gen_server.cast(pid, {:message, {user, server}, text})
      end
    end)
    {:reply, "Message sent.", state}
  end
  
  def handle_call(:users, _from, state) do
    reply = lc {{name, server}, _pid} inlist state, do:
      {name,server}
    {:reply, reply, state}
  end
  
  def handle_call({:who, person, server}, _from, state) do
    {{_u, _s}, pid} = List.keyfind(state, {person, server}, 0)
    reply = :gen_server.call(pid, :get_profile)
    {:reply, reply, state}
  end
  
  def handle_cast(_msg, state) do
    {:noreply, state}
  end
  
  def handle_info(_info, state) do
    {:noreply, state}
  end
  
  def terminate(_reason, _state) do
    {:ok}
  end
  
  def code_change(_old_version, state, _extra) do
    {:ok, state}
  end
end

