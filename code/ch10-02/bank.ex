defmodule Bank do
  @moduledoc """ 
  Manipulate a "bank account" and log messages.
  
  from *Études for Elixir*, O'Reilly Media, Inc., 2014.
  Copyright 2014 by J. David Eisenberg.
  """ 
  @vsn 0.1 

  @doc """
  Create an account with a given balance, and repeatedly
  ask for and perform transactions.
  """
  @spec account(number()) :: nil

  def account(balance) do
    input = IO.gets("D)eposit, W)ithdraw, B)alance, Q)uit: ")
    action = String.upcase(String.first(input))
    if (action != "Q") do
      new_balance = transaction(action, balance)
      account(new_balance)
    end
  end

  @spec transaction(String.t(), number()) :: number()

  def transaction(action, balance) do
    case action do
      "D" ->
        amount = get_number("Amount to deposit: ")
        cond do
          amount >= 10000 ->
            :error_logger.warning_msg("Large deposit $#{amount}\n")
            IO.puts("Your deposit of $#{amount} may be subject to hold.")
            new_balance = balance + amount
            IO.puts("Your new balance is $#{new_balance}") 
          amount < 0 ->
            :error_logger.error_msg("Negative deposit $#{amount}\n")
            IO.puts("Deposits may not be less than zero.")
            new_balance = balance
          amount >= 0 ->
            :error_logger.info_msg("Successful deposit of $#{amount}\n")
            new_balance = balance + amount
            IO.puts("Your new balance is $#{new_balance}")
        end
      "W"->
        amount = get_number("Amount to withdraw: ")
        cond do
          amount > balance ->
            :error_logger.error_msg("Overdraw $#{amount} from $#{balance}\n")
            IO.puts("You cannot withdraw more than your current balance of $#{balance}")
            new_balance = balance
          amount < 0 ->
            :error_logger.error_msg("Negative withdrawal amount $#{amount}\n")
            IO.puts("Withdrawals may not be less than zero.")
            new_balance = balance
          amount >= 0 ->
            :error_logger.info_msg("Successful withdrawal $#{amount}\n")
            new_balance = balance - amount
            IO.puts("Your new balance is $#{new_balance}")
        end
      "B" ->
        :error_logger.info_msg("Balance inquiry $#{balance}\n")
        IO.puts("Your current balance is $#{balance}")
        new_balance = balance
      _ ->
        IO.puts("Unknown command #{action}")
        new_balance = balance
    end
    new_balance
  end        
        
  @doc """
  Present a prompt and get a number from the
  user. Allow either integers or floats.
  """
  @spec get_number(String.t()) :: number()
  
  def get_number(prompt) do
    input = IO.gets(prompt)
    input_str = String.strip(input)
    cond do
      Regex.match?(~r/^[+-]?\d+$/, input_str) ->
        binary_to_integer(input_str)
      Regex.match?(~r/^[+-]?\d+\.\d+([eE][+-]?\d+)?$/, input_str) ->
        binary_to_float(input_str)
      true -> :error
    end
  end     
end
