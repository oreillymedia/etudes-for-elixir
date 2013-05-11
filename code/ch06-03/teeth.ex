defmodule Teeth do
  @moduledoc """ 
  Manipulate a list of lists representing tooth pocket depths.
  
  from *Études for Elixir*, O'Reilly Media, Inc., 2013.
  Copyright 2013 by J. David Eisenberg.
  """ 
  @vsn 0.1 

  @doc "Convenience function for providing a list of lists."
  @spec pocket_depths() :: list(list())
  
  def pocket_depths do
    [[0], [2,2,1,2,2,1], [3,1,2,3,2,3],
    [3,1,3,2,1,2], [3,2,3,2,2,1], [2,3,1,2,1,1],
    [3,1,3,2,3,2], [3,3,2,1,3,1], [4,3,3,2,3,3],
    [3,1,1,3,2,2], [4,3,4,3,2,3], [2,3,1,3,2,2],
    [1,2,1,1,3,2], [1,2,2,3,2,3], [1,3,2,1,3,3], [0],
    [3,2,3,1,1,2], [2,2,1,1,3,2], [2,1,1,1,1,2],
    [3,3,2,1,1,3], [3,1,3,2,3,2], [3,3,1,2,3,3],
    [1,2,2,3,3,3], [2,2,3,2,3,3], [2,2,2,4,3,4],
    [3,4,3,3,3,4], [1,1,2,3,1,2], [2,2,3,2,1,3],
    [3,4,2,4,4,3], [3,3,2,1,2,3], [2,2,2,2,3,3],
    [3,2,3,2,3,2]]
  end
  
  @doc """
  Given a list of list representing tooth pocket depths, return
  a list of the tooth numbers that require attention (any pocket
  depth greater than or equal to four).
  """
  @spec alert(list(list())) :: list()

  def alert(depths) do
    alert(depths, 1, [])
  end

  # Helper function that adds to the list of teeth needing attention
  
  def alert([], _tooth_number, result) do
    Enum.reverse(result)
  end
    
  def alert([h | tail], tooth_number, result) do
    cond do
      Stats.maximum(h) >= 4 ->
        alert(tail, tooth_number + 1, [tooth_number | result])
      true ->
        alert(tail, tooth_number + 1, result)
    end
  end
end

