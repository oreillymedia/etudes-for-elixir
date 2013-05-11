defmodule Calculus do
  @moduledoc """ 
  from *Études for Elixir*, O'Reilly Media, Inc., 2013.
  Copyright 2013 by J. David Eisenberg.
  """ 
  @vsn 0.1 

  @doc """
  Calculate the approximation to the derivative of
  function f and point x by using the mathematical
  definition of a derivative.
  """
  @spec derivative(fun, number) :: number
  
  def derivative(f, x) do
    delta = 1.0e-10
    (f.(x + delta) - f.(x)) / delta
  end
end

