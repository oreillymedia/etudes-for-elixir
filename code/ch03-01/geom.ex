defmodule Geom do
  @moduledoc """ 
  Functions for calculating areas of geometric shapes. 
  from *Études for Elixir*, O'Reilly Media, Inc., 2013.
  Copyright 2013 by J. David Eisenberg.
  """ 
  @vsn 0.1 

  @doc """
  Calculates the area of a shape, given the shape and two of the
  dimensions. Returns the product of its arguments for a rectangle,
  one half the product of the arguments for a triangle, and
  :math.pi times the product of the arguments for an ellipse.
  """
  
  @spec area(atom(), number(), number()) :: number()
  
  def area(:rectangle, length, width) do
    length * width
  end
  
  def area(:triangle, base, height) do
    base * height / 2.0
  end
  
  def area(:ellipse, a, b) do
    :math.pi * a * b
  end
end
