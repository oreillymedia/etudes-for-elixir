defmodule Weather do
  use GenServer.Behaviour
    
  # convenience method for startup
  def start_link do
    :gen_server.start_link({:global, __MODULE__}, __MODULE__, [], [])
  end
  
  # wrapper functions to hide internals of GenServer.
  # These should go with internal functions, but I put them
  # here so they are at the very start of the étude.
  @doc "Connect to another node"
  @spec connect(atom) :: atom
  
  def connect(other_node) do
    case :net_adm.ping(other_node) do
      :pong ->
        IO.puts("Connected to server.")
        :ok
      :pang ->
        IO.puts("Could not connect.")
        :error
    end
  end
  
  @doc "Report weather from a given station."
  @spec report(String.t) :: {atom, list}
  
  def report(station) do
    :gen_server.call({:global, __MODULE__}, station)
  end
  
  @doc "See list of most recently viewed stations"
  @spec recent() :: [String.t]
  
  def recent() do
    result = :gen_server.call({:global, __MODULE__}, :recent)
    IO.puts("Recently visited: #{inspect(result)}")
  end
  
  # callbacks for GenServer.Behaviour
  def init([]) do
    :inets.start()
    {:ok, []}
  end
  
  def handle_call(:recent, _from, state) do
    {:reply, state, state}
  end
  
  def handle_call(request, _from, state) do
    IO.puts(inspect(request))
    {reply, new_state} = get_weather(request, state)
    {:reply, reply, new_state}
  end
  
  def handle_cast(_msg, state) do
    IO.puts("Recently viewed: #{inspect(state)}")
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

  # internal functions
  
  @doc """
  Given a weather station name and the current server state,
  get the weather data for that station, and add the station name
  to the state (the list of recently viewed stations).
  """
  
  def get_weather(station, state) do
    url = "http://w1.weather.gov/xml/current_obs/" <> station
      <> ".xml"
    {status, data} = :httpc.request(to_char_list(url))
    case status do
      :error -> 
        reply = {status, data}
        new_state = state
      :ok ->
        {{_http, code, _message}, _attrs, xml_as_chars} = data
        case code do
          200 ->
            xml = to_string(xml_as_chars)
            reply = {:ok,
              (lc item inlist [:location, :observation_time_rfc822,
              :weather, :temperature_string], do:
                get_content(item, xml))}
            # remember only the last 10 stations
            new_state = [station | Enum.take(state, 9)]
          _ ->
            reply = {:error, code}
            new_state = state
        end
    end
    {reply, new_state}
  end
  
  
  # Given an element name (as an atom) and an XML string,
  # return a tuple with the element name and that element's text
  # content.
  
  @spec get_content(atom, String.t) :: {atom, String.t}
  
  defp get_content(element_name, xml) do
    {_, pattern} = Regex.compile(
    "<#{element_name}>([^<]+)</#{atom_to_binary(element_name)}>")
    result = Regex.run(pattern, xml)
    case result do
      [_all, match] -> {element_name, match}
      nil -> {element_name, nil}
    end
  end

end
