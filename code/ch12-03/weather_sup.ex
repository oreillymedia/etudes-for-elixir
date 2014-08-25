defmodule WeatherSup do
  use Supervisor

  # convenience method for startup
  def start_link do
    Supervisor.start_link(__MODULE__, [], [{:name, __MODULE__}])
  end

  # supervisor callback
  def init([]) do
    child = [worker(Weather, [], [])]
    supervise(child, [{:strategy, :one_for_one}, {:max_restarts, 1},
      {:max_seconds, 5}])
  end
  
  # Internal functions (none here)
  
end
