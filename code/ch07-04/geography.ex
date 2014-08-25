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

defmodule Country do
  defstruct name: "", language: "", cities: []
end

defmodule Geography do
  @moduledoc """ 
  Using files and structures.
  from *Études for Elixir*, O'Reilly Media, Inc., 2014.
  Copyright 2014 by J. David Eisenberg.
  """ 
  @vsn 0.1 
  
  @doc """
  Open a file whose name is given in the first argument.
  The file contains country name and primary language, 
  followed by (for each country) lines giving the name
  of a city, its population, latitude, and longitude.
  Construct a Country structure for each country containing
  the cities in that country.
  """
  @spec make_geo_list(String.t) :: Country.t
  
  def make_geo_list(file_name) do
    {_result, device} = File.open(file_name, [:read, :utf8])
    process_line(device, [])
  end
  
  @doc """
  Find the total population of all cities in the list
  that are in countries with a given primary language.
  """
  
  @spec total_population([Country], String.t) :: integer

  def total_population(geo_list, language) do
    total_population(geo_list, language, 0)
  end


  defp total_population([], _language, total) do
    total
  end

  defp total_population([head|tail], language, total) do
    if (head.language == language) do
      total_population(tail, language, subtotal(head.cities, 0))
    else
      total_population(tail, language, total)
    end
  end
  
  defp subtotal([], accumulator) do
    accumulator
  end
  
  defp subtotal([head | tail], accumulator) do
    subtotal(tail, accumulator + head.population)
  end
    
  # Read next line from file; if not end of file, process
  # the data on that line. Recursively read through end of file.
  
  defp process_line(device, geo_list) do
    data = IO.read(device, :line)
    case data do
      :eof ->
        File.close(device)
        geo_list
      _ ->
        info = String.split(String.strip(data), ",")
        updated_list = process_info(info, geo_list)
        process_line(device, updated_list)
    end
  end
  
  # If the info has only two elements, start a new country
  
  defp process_info([country, language], geo_list) do
   [%Country{name: country, language: language, cities: []}
      | geo_list]
  end
  
  # If it has four elements, it's a city; add it to the list of
  # cities. Notice the code for updating the cities field in the
  # head of the list.
  
  defp process_info([city, populn, lat, long], [hd|tail]) do
    new_cities = [%City{name: city, population: String.to_integer(populn),
      latitude: String.to_float(lat), longitude: String.to_float(long)} |
      hd.cities]
    [%Country{ hd | cities: new_cities} | tail]
  end  
    
end