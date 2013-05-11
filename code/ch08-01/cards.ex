defmodule Cards do
  @moduledoc """ 
  Functions for simulating a deck of cards.
  
  from *Études for Elixir*, O'Reilly Media, Inc., 2013.
  Copyright 2013 by J. David Eisenberg.
  """ 
  @vsn 0.1 

  @doc """
  Shuffle a list into random order using the Fisher-Yates method.
  """
  @spec shuffle(list) :: list
  
  def shuffle(list) do
    :random.seed(:erlang.now())
    shuffle(list, [])
  end
  
  # The helper function takes a list to shuffle as its first
  # argument and the accumulated (shuffled) list as its second
  # argument.
  
  # When there are no more cards left to shuffle,
  # return the accumulated list.
  
  def shuffle([], acc) do
    acc
  end
  
  # This is easier to understand if you look at it as a
  # physical process. Split the deck at a random point.
  # Put the part above the "split point" aside (leading), and
  # take the first card (h) off the part below the split (t).
  # That first card goes onto a new pile ([h | acc]).
  # Now put together the part above the split and the
  # part below the split (leading ++ t) and go through
  # the process with the deck (which is now has one less card).
  # This keeps going until you run out of cards to shuffle;
  # at that point, all the cards will have gotten to the
  # new pile, and that's your shuffled deck.

  def shuffle(list, acc) do
    {leading, [h | t]} =
      Enum.split(list, :random.uniform(Enum.count(list)) - 1)
      shuffle(leading ++ t, [h | acc])
  end

  @doc """
  Create a deck of 52 tuples in the form [{"A", "Clubs"},
  {"A", "Diamonds"}...]
  """
  @spec make_deck(list, list) :: list(tuple)
  
  def make_deck(values, suits) do
    lc value inlist values, suit inlist suits, do:
      {value, suit}
  end
  
end

