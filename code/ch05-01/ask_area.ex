defmodule AskArea do

  @doc """
  Requests a character for the name of a shape,
  numbers for its dimensions, and calculates shape's area.
  The characters are R for rectangle, T for triangle,
  and E for ellipse. Input is allowed in either upper
  or lower case.
  """

  @spec area() :: number()
  
  def area() do
    input = IO.gets("R)ectangle, T)riangle, or E)llipse: ")
    shape = char_to_shape(hd(input))
    {d1, d2} = case shape do
      :rectangle -> get_dimensions("Enter width > ", "Enter height > ")
      :triangle -> get_dimensions("Enter base > ", "Enter height > " )
      :ellipse -> get_dimensions("Enter major radius > ",
        "Enter minor radius > ")
      :unknown -> {0, 0}
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
      character == ?r or character == ?R -> :rectangle
      character == ?t or character == ?T -> :triangle
      character == ?e or character == ?E -> :ellipse
      true -> :unknown
    end
  end
  
  @doc """
  Present a prompt and get a number from the
  user. Allow either integers or floats.
  """
  @spec get_number(String.t()) :: number()
  
  def get_number(prompt) do
    input = IO.gets(prompt)
    binary_to_integer(String.strip(list_to_binary(input)))
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
  
  def calculate(shape, d1, d2) do
    cond do
      shape == :unknown -> IO.puts("Unknown shape.")
      d1 < 0 or d2 < 0 ->
        IO.puts("Both numbers must be greater than or equal to zero.")
      true -> Geom.area(shape, d1, d2)
    end
  end
end
