defmodule Dijkstra do

  @moduledoc """
  Recursive function for calculating GCD of two numbers using
  Dijkstra's algorithm.
  from *Études for Elixir*, O'Reilly Media, Inc., 2013.
  Copyright 2013 by J. David Eisenberg.
  """
  @vsn 0.1

  @doc """
  Calculates the greatest common divisor of two integers.
  Uses Dijkstra's algorithm, which does not require any division.
  """
  
  @spec gcd(number(), number()) :: number()

  def gcd(m, n) do
    cond do
      m == n -> m
      m > n -> gcd(m - n, n)
      true -> gcd(m, n - m)
    end
  end
end
