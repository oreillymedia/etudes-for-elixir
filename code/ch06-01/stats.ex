defmodule Stats do
  @moduledoc """ 
  Functions for calculating basic statistics.
  
  from *Études for Elixir*, O'Reilly Media, Inc., 2013.
  Copyright 2013 by J. David Eisenberg.
  """ 
  @vsn 0.1 

  @doc "Recursively find the minimum entry in a list of numbers."
  
  @spec minimum([number]) :: number
  
  def minimum(list) do
    [head | tail] = list
    minimum(tail, head)
  end
  
  # When there are no more numbers, return the result.
  
  @spec minimum([number], number) :: number
  
  defp minimum([], result) do
    result
  end
  
  # If the current result is less than the first item in the list,
  # keep it as the result and recursively look at the remainder of the list.
  # Note that you can use a variable assigned as part of a cons in a guard.
  
  defp minimum([head | tail], result) when result < head do
    minimum(tail, result)
  end
  
  # Otherwise, the head of the list becomes the new minimum,
  # and recursively look at the remainder of the list.
  
  defp minimum([head | tail], _result) do
    minimum(tail, head)
  end
  
end
