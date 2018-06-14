defmodule NonFP do
  @moduledoc """ 
  Use non-functional-programming constructs to create
  a list of lists with random entries.
  
  from *Études for Elixir*, O'Reilly Media, Inc., 2013.
  Copyright 2013 by J. David Eisenberg.
  """ 
  @vsn 0.1 

  @doc """
  Generate a list of list representing pocket depths for a set of teeth.
  The first argument is a character list consisting of T and F for teeth
  that are present or absent. The second argument is the probability that
  any given tooth will be good.
  """
  @spec generate_pockets(list, number) :: list(list)
  
  def generate_pockets(tooth_list, prob_good) do
    :random.seed(:erlang.timestamp())
    generate_pockets(tooth_list, prob_good, [])
  end
  
  # When the tooth list is empty, return the final list of lists
  # If a tooth is present, generate a set of six depths and add it
  # to the result; otherwise add a [0] to the result.
  
  defp generate_pockets([], _, result) do
    Enum.reverse(result)
  end
  
  defp generate_pockets([head | tail], prob_good, result) when head == ?T do
    tooth = generate_tooth(prob_good)
    generate_pockets(tail, prob_good, [tooth | result])
  end
  
  defp generate_pockets([_head | tail], _prob_good, result) do
    generate_pockets(tail, _prob_good, [[0] | result])
  end
  
  @doc """
  Generate a set of six pocket depths for a tooth, given a probability
  that a tooth is good.
  """
  @spec generate_tooth(number) :: list(number)
  
  def generate_tooth(prob_good) do
    r = :random.uniform()
    if (r < prob_good) do
      base_depth = 2
    else
      base_depth = 3
    end
    generate_tooth(base_depth, 6, [])
  end
  
  def generate_tooth(_base, 0, result) do
    result
  end
  
  def generate_tooth(base, n, result) do
    delta = :random.uniform(3) - 2  # result will be -1, 0, or 1
    generate_tooth(base, n - 1, [base + delta | result])
  end
  
  def test_pockets() do
    tlist = 'FTTTTTTTTTTTTTTFTTTTTTTTTTTTTTTT'
    big_list = generate_pockets(tlist, 0.75)
    print_pockets(big_list)
  end
  
  def print_pockets([]), do: IO.puts("Finished.")
  
  def print_pockets([head | tail]) do
    IO.puts(inspect(head))
    print_pockets(tail)
  end
end

