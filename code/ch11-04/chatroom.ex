defmodule Chatroom do
  use GenServer.Behaviour
  
  # convenience method for startup
  def start_link do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end
  
  # callbacks for GenServer.Behaviour
  def init([]) do
    {:ok, []}
  end

  # A login request gets a user name and node (server).
  # Check that user name and server are unique; if they are,
  # add that person's user name, server, and pid to the state.
  
  def handle_call({:login, user, server}, from, state) do
    {pid, _reference} = from
    key = {user, server}
    IO.puts("#{user} #{server} logging in from #{inspect(pid)}")
    if List.keymember?(state, key, 0) do
      reply = {:error, "#{user} #{server} already logged in"}
      new_state = state
    else
      new_state = [{key,pid} | state]
      reply = {:ok, "#{user}@#{server} logged in."}
    end
    {:reply, reply, new_state}
  end
  

  # If a person isn't logged in, note error;
  # otherwise, delete person from the state.
  
  def handle_call(:logout, from, state) do
    {pid, _reference} = from
    result = List.keyfind(state, pid, 1)
    case result do
      nil ->
        reply = {:error, "Not logged in."}
        new_state = state
      {{user, server}, _pid} ->
        new_state = List.keydelete(state, pid, 1)
        reply = {:ok, "#{user}@#{server} logged out."}
    end
    {:reply, reply, new_state}
  end
  
  # Send a message to all other participants.
  # First, find sender's name and node. Then
  # go through the list of participants and send each of them
  # the message via :gen_server.cast.
  #
  # Don't send a message to the originator (though if this were
  # connected to a GUI, it would be useful to do so), as the
  # originator wants to see the message she has typed in a text area
  # as well as in the chat window.
  
  def handle_call({:say, text}, from, state) do
    {from_pid, _ref} = from
    
    # get sender's name and server
    person = List.keyfind(state, from_pid, 1)
    
    case person do
      {{from_user, from_server}, _pid} ->
        Enum.each(state, fn(item) ->
          {{_user, _server}, pid} = item
          if pid != from_pid do
            :gen_server.cast(pid, {:message,
              {from_user, from_server}, text})
          end
        end)
        reply = "Message sent."
      nil ->
        reply = "Unknown sender pid #{inspect(from_pid)}"
    end
    {:reply, reply, state}
  end
  
  # Return a list of all the users and their servers
  
  def handle_call(:users, _from, state) do
    reply = lc {{name, server}, _pid} inlist state, do:
      {name,server}
    {:reply, reply, state}
  end
  
  # Get the profile of a person at a given server
  
  def handle_call({:who, person, server}, _from, state) do
    case List.keyfind(state, {person, server}, 0) do
      {{_u, _s}, pid}  ->
        reply = :gen_server.call(pid, :get_profile)
      nil ->
        reply = "Cannot find #{person} #{server}"
    end
    {:reply, reply, state}
  end
  
  # Catchall to handle any errant calls to the server.
  
  def handle_call(item, from, state) do
    IO.puts("Unknown #{inspect(item)} from #{inspect(from)}")
    {:reply, "unknown", state}
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

