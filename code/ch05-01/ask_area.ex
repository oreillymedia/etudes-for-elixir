defmodule AskArea do

  def area() do
    input = IO.gets("R)ectangle, T)riangle, or E)llipse: ")
    shape_atom = char_to_shape(hd(input))
    shape_atom
  end
  
  def char_to_shape(character) do
    cond do
      character == ?r or character == ?R -> :rectangle
      character == ?t or character == ?T -> :triangle
      character == ?e or character == ?E -> :ellipse
      true -> :unknown
    end
  end
  
  def get_number(prompt) do
    input = IO.gets(prompt)
  end
end
