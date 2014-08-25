defprotocol Valid do
  @doc "Returns true if data is considered valid"
  def valid?(data)
end

defmodule City do
  defstruct name: "",  population: 0, latitude: 0.0, longitude: 0.0
end

defimpl Valid, for: City do
  def valid?(%City{population: p, latitude: lat, longitude: lon}) do
    p > 0 && lat >= -90 && lat <= 90 &&
    lon >= -180 && lon <= 180
  end
end

defimpl Inspect, for: City do
  import Inspect.Algebra
  
  def inspect(item, _options) do
    lat = if (item.latitude < 0) do
      concat(to_string(Float.round(abs(item.latitude * 1.0), 2)), "°S")
    else
      concat(to_string(Float.round(item.latitude * 1.0, 2)), "°N")
    end
    
    lon = if (item.longitude < 0) do
      concat(to_string(Float.round(abs(item.longitude * 1.0), 2)), "°W")
    else
      concat(to_string(Float.round(item.longitude * 1.0, 2)), "°E")
    end
    
    msg = concat([item.name, break,
      "(", to_string(item.population), ")", break,
      lat, break, lon])
    pretty(msg, 80)
  end
end

