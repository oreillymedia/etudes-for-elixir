defmodule Powers do
  import Kernel, except: [raise: 2, raise: 3]

  @moduledoc """
  Function for raising a number to an integer power.
  from *Études for Elixir*, O'Reilly Media, Inc., 2013.
  Copyright 2013 by J. David Eisenberg.
  """
  @vsn 0.1

  @doc """
  Raise a number x to an integer power n.
  Any number to the power 0 equals 1.
  Any number to the power 1 is that number itself.
  When n is positive, x^n is equal to x times x^(n - 1)
  When n is negative, x^n is equal to 1.0 / x^n
  """
  
  @spec raise(number(), number()) :: number()

  def raise(_, 0) do
    1
  end
  
  def raise(x, n) when n < 0 do
    1.0 / raise(x, -n)
  end

  def raise(x, n) when n > 0 do
    raise(x, n, 1)
  end
  
  @spec raise(number(), number(), number()) :: number()

  def raise(_x, 0, accumulator) do
    IO.puts("n = 0; result is #{accumulator}")
    accumulator
  end
  
  def raise(x, n, accumulator) do
    IO.puts("Enter with x = #{x}, n = #{n}, accumulator = #{accumulator}")
    raise(x, n - 1, x * accumulator)
  end
end
