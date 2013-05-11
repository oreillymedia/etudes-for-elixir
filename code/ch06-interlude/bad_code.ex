defmodule BadCode do
 @moduledoc """ 
  Functions for calculating basic statistics.
  
  from *Études for Elixir*, O'Reilly Media, Inc., 2013.
  Copyright 2013 by J. David Eisenberg.
  """ 

  @doc """
  Attempt to square a list of numbers.
  This is the worst approach to use, as it generates improper lists.
  """
  @spec squares(list) :: list
  
  def squares(numbers), do: squares(numbers, [])
  
  def squares([], result), do: result

  def squares([h | t], result), do: squares(t, [result | h * h])

  @doc """
  Another attempt to square a list of numbers.
  This is a bad approach to use; at least you get a list,
  but it's a nested list.
  """

  @spec squares2(list) :: list

  def squares2(numbers), do: squares2(numbers, [])

  def squares2([], result), do: List.flatten(result)
  
  def squares2([h | t], result), do: squares2(t, [result | [h * h]])
end
