defmodule Person do
  use GenServer.Behaviour
  
  defrecord State, server: nil, profile: nil
  
  # convenience method for startup
  def start_link(chatroom_server) do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__,
      [chatroom_server], [])
  end
  
  # callbacks for GenServer.Behaviour
  def init([chatroom_server]) do
    {:ok, State.new(server: {Chatroom, chatroom_server},
      profile: HashDict.new)}
  end
  
  def handle_call(:get_chat_server, _from, state) do
    {:reply, state.server, state}
  end
  
  def handle_call(:get_profile, _from, state) do
    {:reply, state.profile, state}
  end

  def handle_call({:login, user_name}, _from, state) do
    :gen_server.call(state.server, {:login, user_name, node()})
    {:reply, "Sent login request", state}
  end

  def handle_call({:say, text}, _from, state) do
    reply = :gen_server.call(state.server, {:say, text})
    {:reply, reply, state} 
  end
  
  def handle_call(:logout, _from, state) do
    reply = :gen_server.call(state.server, :logout)
    {:reply, reply, state}
  end
  
  def handle_call({:set_profile, key,value}, _from, state) do
    new_profile = HashDict.put(state.profile, key, value)
    reply = {:ok, "Added #{key}/#{value} to profile"}
    {:reply, reply, State.new(server: state.server, profile: new_profile) }
  end
  
  def handle_call(item, from, state) do
    IO.puts("Unknown message #{inspect(item)} from #{inspect(from)}")
    {:reply, "unknown msg to person", state}
  end
  
  def handle_cast({:message, {user, server}, text}, state) do
    IO.puts("#{user} (#{server}) says: #{text}")
    {:noreply, state}
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
  
  # Internal convenience functions
  def chat_server() do
    :gen_server.call(__MODULE__, :get_chat_server)
  end
  
  # These requests can go straight to the chat server,
  # because it doesn't care who they're from.
  
  def users do
    :gen_server.call(chat_server, :users)
  end
  
  def who(user_name, user_node) do
    :gen_server.call(chat_server, {:who, user_name, user_node})
  end
  
  #
  # Forward these requests to the person server,
  # because they have to send the request to the chat room,
  # not the shell!
  #
  def login(user_name) when is_atom(user_name) do
    login(atom_to_binary(user_name))
  end
  
  def login(user_name) do
    :gen_server.call(__MODULE__, {:login, user_name})
  end
  
  def logout do
    :gen_server.call(__MODULE__, :logout)
  end
  
  def say(text) do
    :gen_server.call(__MODULE__, {:say, text})
  end

  #
  # This request is local to the Person server.
  #  
  def set_profile(key, value) do
    :gen_server.call(__MODULE__, {:set_profile, key, value})
  end
end

