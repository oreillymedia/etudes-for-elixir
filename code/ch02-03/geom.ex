defmodule Geom do
  @moduledoc """ 
  Functions for calculating areas of geometric shapes.
  
  from *Études for Elixir*, O'Reilly Media, Inc., 2013.
  Copyright 2013 by J. David Eisenberg.
  """ 
  @vsn 0.1 

  @doc """
  Calculates the area of a rectangle, given the length and width.
  Returns the product of its arguments. The default value for
  both arguments is 1.
  """
  
  @spec area(number(), number()) :: number()
  
  def area(length \\ 1, width \\ 1) do
    length * width
  end
end
