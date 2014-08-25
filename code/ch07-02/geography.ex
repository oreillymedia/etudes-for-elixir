defmodule City do
  defstruct name: "",  population: 0, latitude: 0.0, longitude: 0.0
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
  Open a file with columns country code, city, population,
  latitude, and longitude.
  Construct a Country structure for each country containing
  the cities in that country.
  """
  @spec make_geo_list(String.t) :: Country.t
  
  def make_geo_list(file_name) do
    {_result, device} = File.open(file_name, [:read, :utf8])
    process_line(device, [])
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