defmodule GoodCode do
 @moduledoc """ 
  Functions for calculating basic statistics.
  
  from *Études for Elixir*, O'Reilly Media, Inc., 2013.
  Copyright 2013 by J. David Eisenberg.
  """ 

  @doc """
  Successfully square a list of numbers.
  This is the correct approach to use. When you use | to construct
  a list, the new item always comes at the head, and the old list is last.
  """
  @spec squares(list) :: list
  
  def squares(numbers), do: squares(numbers, [])
  
  def squares([], result), do: Enum.reverse(result)

  def squares([h | t], result), do: squares(t, [h * h | result])
end
