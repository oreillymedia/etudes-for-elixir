%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Implement a bank account that logs its transactions.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(bank).
-export([account/1]).

-spec(account(pid()) -> number()).

%% @doc create a client and give it the process ID for an account
account(Balance) ->
  Input = io:get_line("D)eposit, W)ithdraw, B)alance, Q)uit: "),
  Action = hd(Input),
  
  case Action of
    $D ->
      Amount = get_number("Amount to deposit: "),
      NewBalance = transaction(deposit, Balance, Amount);
    $W ->
      Amount = get_number("Amount to withdraw: "),
      NewBalance = transaction(withdraw, Balance, Amount);
    $B ->
      NewBalance = transaction(balance, Balance);
    $Q ->
      NewBalance = Balance;
    _ ->
      io:format("Unknown command ~c~n", [Action]),
      NewBalance = Balance
  end,
  if
    Action /= $Q ->
    account(NewBalance);
    true -> true
  end.
  

%% @doc Present a prompt and get a number from the
%% user. Allow either integers or floats.
get_number(Prompt) ->
  Str = io:get_line(Prompt),
  {Test, _} = string:to_float(Str),
  case Test of
    error -> {N, _} = string:to_integer(Str);
    _ -> N = Test
  end,
  N.

transaction(Action, Balance, Amount) ->
  case Action of
    deposit ->
      if
        Amount >= 10000 ->
          error_logger:warning_msg("Excessive deposit ~p~n", [Amount]),
          io:format("Your deposit of $~p may be subject to hold.", [Amount]),
          io:format("Your new balance is ~p~n", [Balance + Amount]),
          NewBalance = Balance + Amount;
        Amount < 0 ->
          error_logger:error_msg("Negative deposit amount ~p~n", [Amount]),
          io:format("Deposits may not be less than zero."),
          NewBalance = Balance;
        Amount >= 0 ->
          error_logger:info_msg("Successful deposit ~p~n", [Amount]),
          NewBalance = Balance + Amount,
          io:format("Your new balance is ~p~n", [NewBalance])
      end;
    withdraw ->
      if
        Amount > Balance ->
          error_logger:error_msg("Overdraw ~p from balance ~p~n", [Amount,
            Balance]),
          io:format("You cannot withdraw more than your current balance of ~p.~n",
            [Balance]),
          NewBalance = Balance;
        Amount < 0 ->
          error_logger:error_msg("Negative withdrawal amount ~p~n", [Amount]),
          io:format("Withdrawals may not be less than zero."),
          NewBalance = Balance;
        Amount >= 0 ->
          error_logger:info_msg("Successful withdrawal ~p~n", [Amount]),
          NewBalance = Balance - Amount,
          io:format("Your new balance is ~p~n", [NewBalance])
      end
  end,
  NewBalance.

transaction(balance, Balance) ->
  error_logger:info_msg("Balance inquiry ~p~n", [Balance]),
  Balance.
   


