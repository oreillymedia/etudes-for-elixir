defmodule Cards do
  @moduledoc """ 
  Functions for simulating a deck of cards.
  
  from *Études for Elixir*, O'Reilly Media, Inc., 2013.
  Copyright 2013 by J. David Eisenberg.
  """ 
  @vsn 0.1 

  @doc """
  Create a deck of 52 tuples in the form [{"A", "Clubs"},
  {"A", "Diamonds"}...]
  """
  @spec make_deck() :: list(tuple)
  
  def make_deck() do
    lc value inlist ["A", 2, 3, 4, 5, 6, 7, 8, 9, "J", "Q", "K"],
      suit inlist ["Clubs", "Diamonds", "Hearts", "Spades"], do:
      {value, suit}
  end
end

