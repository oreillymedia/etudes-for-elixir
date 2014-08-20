defmodule ListComp do

  # private function to return a list of people
  
  @spec get_people() :: list(tuple())
  
  defp get_people() do
    [{"Federico", "M", 22}, {"Kim", "F", 45}, {"Hansa", "F", 30},
      {"Tran", "M", 47}, {"Cathy", "F", 32}, {"Elias", "M", 50}]
  end
  
  @doc """
  Select all males older than 40 from a list of tuples giving
  name, gender, and age.
  """
  @spec older_males() :: list(tuple())
  
  def older_males() do
      for {name, gender, age} <- get_people(), age > 40, gender == "M", do: {name, gender, age}
    end
  end
  
  @doc"""
  Select all people who are male or older than 40 from a list of
  tuples giving name, gender, and age.
  """
  @spec older_or_male() :: list
  def older_or_male() do
    for {name, gender, age} <- get_people(), age > 40, gender == "M", do: {name, gender, age}
  end
end

  

