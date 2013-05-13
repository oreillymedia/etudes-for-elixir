defmodule WeatherSup do
  use Supervisor.Behaviour

  # convenience method for startup
  def start_link do
    :supervisor.start_link({:local, __MODULE__}, __MODULE__, [])
  end

  # supervisor callback
  def init([]) do
    child = [worker(Weather, [], [])]
    supervise(child, [{:strategy, :one_for_one}, {:max_restarts, 1},
      {:max_seconds, 5}])
  end
  
  # Internal functions (none here)
  
end
