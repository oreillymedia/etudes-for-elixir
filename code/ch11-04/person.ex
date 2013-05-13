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
    :inets.start()
    {:ok, State.new(server: {Chatroom, chatroom_server},
      profile: HashDict.new)}
  end
  
  def handle_call(:get_chat_node, _from, state) do
    {:reply, state.server, state}
  end
  
  def handle_call(:get_profile, _from, state) do
    {:reply, state.profile, state}
  end
  
  def handle_call({:profile, key,value}, _from, state) do
    new_profile = HashDict.put(state.profile, key, value)
    reply = {:ok, "Added #{key}/#{value} to profile"}
    {:reply, reply, State.new(server: state.server, profile: new_profile) }
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
  
  def login(user_name) when is_atom(user_name) do
    login(atom_to_binary(user_name))
  end
  
  def login(user_name) do
    :gen_server.call(chat_server, {:login, user_name})
  end
  
  def logout do
    :gen_server.call(chat_server, :logout)
  end
  
  def say(text) do
    :gen_server.call(chat_server, {:say, text})
  end
  
  def users do
    :gen_server.call(chat_server, :users)
  end
    
  def who(user_name, user_node) do
    :gen_server.call(chat_server, {:who, user_name, user_node})
  end
  
  def profile(key, value) do
    :gen_server.call(__MODULE__, {:profile, key, value})
  end
end

