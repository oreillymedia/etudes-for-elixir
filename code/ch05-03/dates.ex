defmodule Dates do
  @moduledoc """ 
  Functions for manipulating calendar dates.
  
  from *Études for Elixir*, O'Reilly Media, Inc., 2013.
  Copyright 2013 by J. David Eisenberg.
  """ 
  @vsn 0.1 

  @doc """
  Takes a string in ISO date format (yyyy-mm-dd) and
  returns a list of integers in form [year, month, day].
  """
  @spec date_parts(list) :: list

  def date_parts(date_str) do
    [y_str, m_str, d_str] = String.split(date_str, ~r/-/)
    [String.to_integer(y_str), String.to_integer(m_str),
      String.to_integer(d_str)]
  end
end
