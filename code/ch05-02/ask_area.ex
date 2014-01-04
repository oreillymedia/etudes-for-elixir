defmodule AskArea do
  @moduledoc """ 
  Validate input using regular expressions.
  
  from *Études for Elixir*, O'Reilly Media, Inc., 2013.
  Copyright 2013 by J. David Eisenberg.
  """ 
  @vsn 0.1 

  @doc """
  Requests a character for the name of a shape,
  numbers for its dimensions, and calculates shape's area.
  The characters are R for rectangle, T for triangle,
  and E for ellipse. Input is allowed in either upper
  or lower case. For unknown shapes, the first "dimension" will
  be the unknown character.
  """

  @spec area() :: number()
  
  def area() do
    input = IO.gets("R)ectangle, T)riangle, or E)llipse: ")
    shape = char_to_shape(String.first(input))
    {d1, d2} = case shape do
      :rectangle -> get_dimensions("width", "height")
      :triangle -> get_dimensions("base ", "height" )
      :ellipse -> get_dimensions("major radius", "minor radius")
      :unknown -> {String.first(input), 0}
    end
    calculate(shape, d1, d2)
  end
  
  @doc """
  Given a character, returns an atom representing the
  specified shape (or the atom unknown if a bad character is given).
  """
 
  @spec char_to_shape(char()) :: atom()
  
  def char_to_shape(character) do
    cond do
      String.upcase(character) == "R" -> :rectangle
      String.upcase(character) == "T" -> :triangle
      String.upcase(character) == "E" -> :ellipse
      true -> :unknown
    end
  end
  
  @doc """
  Present a prompt and get a number from the
  user. Allow either integers or floats.
  """
  @spec get_number(String.t()) :: number()
  
  def get_number(prompt) do
    input = IO.gets("Enter #{prompt} > ")
    input_str = String.strip(input)
    cond do
      Regex.match?(%r/^[+-]?\d+$/, input_str) ->
        binary_to_integer(input_str)
      Regex.match?(%r/^[+-]?\d+\.\d+([eE][+-]?\d+)?$/, input_str) ->
        binary_to_float(input_str)
      true -> :error
    end
  end
  
  @doc """
  Get dimensions for a shape. Input are the two prompts,
  output is a tuple {Dimension1, Dimension2}.
  """
  @spec get_dimensions(String.t(), String.t()) :: {number(), number()}
  
  def get_dimensions(prompt1, prompt2) do
    n1 = get_number(prompt1)
    n2 = get_number(prompt2)
    {n1, n2}
  end
  
  @doc """
  Calculate area of a shape, given its shape and dimensions.
  Handle errors appropriately.
  """
  @spec calculate(atom(), number(), number()) :: number()
  
  def calculate(_shape, :error, _) do
    IO.puts("First number is non-numeric")
  end
  
  def calculate(_shape, _, :error) do
    IO.puts("Second number is non-numeric")
  end
  
  def calculate(shape, d1, d2) do
    cond do
      shape == :unknown -> IO.puts("Unknown shape #{d1}")
      d1 < 0 or d2 < 0 ->
        IO.puts("Both numbers must be greater than or equal to zero.")
      true -> Geom.area(shape, d1, d2)
    end
  end
end
